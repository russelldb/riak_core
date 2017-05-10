# The Ring and Claim

## Summary

It is common when presenting Riak to throw in a picture of the ring and assume everyone understands what this means.

![](ibm_ring.png "IBM Presents the Ring")

However, the ring, and in particular the process by which nodes claim from the ring isn't necessarily obvious to everyone.  Perhaps sometimes even the speaker doesn't really understand what it all means.  

This is an attempt to explain the problems associated with claiming from a ring, examine the current implementations of claim within Riak and some issues with it, and then discuss areas for improvement.

## Overview of The Ring - the Basics

At design time of a Riak cluster, it is necessary to set the [size of the ring](http://docs.basho.com/riak/kv/2.2.3/configuring/basic/#ring-size) to a power of two.  Getting this right at the start is important, as although there exists the potential to resize a ring post go-live, it is difficult to do in a risk-free way under production load.  Currently, ring re-sizing is not a solved problem.  Guidance on how to size a ring in advance is [vague](http://docs.basho.com/riak/kv/2.2.3/setup/planning/cluster-capacity/#ring-size-number-of-partitions).  Currently, correctly setting the size of a ring to avoid resizing is not a solved problem.

The size of the ring simply defines the number of separate (and equal) partitions of the hash space there will be.  Each Bucket/Key combination will be hashed before storage, and hashed before fetching, and the outcome of that consistent hashing algorithm will determine the partition to which that Bucket/Key pair belongs.  Each partition has an ID, which is the floor hash value for that partition: to belong to a partition a Bucket/Key pair must hash to a value which is greater than the floor, and less than or equal to the floor of the next partition.  The "first" partition is ID 0, but this is only the first in the sense it has the lowest Partition ID - it has no special role in the ring.

Within a Riak cluster there are individual databases known as vnodes, where each vnode is associated with a single Partition ID.  The more partitions, the easier it is to spread load smoothly throughout larger and larger clusters.  The less partitions, the lower the overhead of running all the vnodes necessary to support each partition.  As the node count increases, so does the optimal number of vnodes.

Every bucket within Riak has an n-val which indicates how many vnodes each value should normally be stored in, and retrieved from.  When a key is to be stored or retrieved the Bucket/Key is hashed and then the operation will be performed against the vnode  with the Partition ID which the Key hashes to, plus the next n-1 vnodes in numerical order of Partition ID.  If one of these vnodes is known to be unavailable at commencement of the operation (Riak will gossip the status of vnodes around the cluster), then the next vnode in numerical order will also be used as a fallback (i.e. the node n partitions along from the one the key hashed to).  If more vnodes expected to be used are unavailable, more fallbacks will be used.  

If the database operation doesn't know the keys it is interested in, for example in a secondary index query, it must ask every nth vnode - where n is the n-val for the bucket related to that query.  If there is a n-val of three, that means there are three different coverage plans in a healthy cluster that could be used for the query, with the offset being chosen at random to determine which plan will be used.  To be explicit, there will be one coverage plan based on the 1st partition, the 4th partition, the 7th partition etc - and two more coverage plans where the partitions used are right shifted by 1 and 2 partitions respectively.

If a vnode is unavailable for a coverage query, using a fallback node is not a desirable answer.   Coverage queries are run with r=1 (only one vnode is checked for every part of the keyspace), so fallback nodes are unsafe as they only contain the data since the cluster change event occurred for some of those partitions.  So the coverage planner must assess the partitions which are not covered by the plan, and find other vnodes that can fill the role of the missing vnodes.  

For most vnodes in a coverage plan, the coverage plan is interested in all the answers present in that vnode, but where the coverage plan is looking for a vnode to fill a gap in the coverage, the query is passed with a partition filter so that only results in specific partitions are fetched.  Using the partition filter avoids unnecessary duplication of results.  This same filter is used for partitions at the tail of the coverage plan, which would otherwise return results that have already been found from the front of the plan (as the ring-size will not be divisible by three).

## Overview of the Ring - the Claiming problem

If we have a ring - which is really a list of partitions of a key space, which are mapped in turn to vnodes (individual key-value stores): the key question is how do we distribute these vnodes around the physical cluster.  

Riak has no specifically assigned roles (i.e. there is no master controlling node), so in theory distribution of vnodes can be managed by a process of nodes claiming the number of vnodes they want, and a consensus being reached within the cluster what the up-to-date set of claims are.  Different nodes can propose changes to the ring, and detection of conflicts between different proposals is managed using vector clocks.  In practice, such changes to claim are made through administrative action using cluster plans, and the node on which the cluster plan is made will make claims on the ring on behalf of the nodes involved in the change.

There are some properties of this claiming process which are required, and are listed here in decreasing order of obviousness:

- vnodes within n partitions of each other should not be mapped to the same physical nodes (so that rights in a healthy cluster are always safely distributed across different nodes);

- vnodes at the tail of the partition list, should not be mapped to the same physical nodes as vnodes at the front of the partition list (i.e. the above property must hold as the tail wraps around to the start);

- there should be an even distribution of vnodes across the physical nodes;

- transitions from one cluster state to another should minimise the number of transfers required;

- all coverage plans should be evenly spread across the physical nodes (so that expensive queries don't cause a subset of slow nodes);

- the above properties should be upheld if nodes are added one-by-one, or joined in bulk;

- vnodes should ideally be spaced as a far as possible, not just minimally.

Each of these properties will be examined in turn, and the explanation of those properties will hint at both how Riak may fail to meet some of these properties, and how that failure could be resolved.  The specific problems in Riak and the proposed solution will be explained in a later section.

### Proximity of vnodes

The mapping of vnodes to nodes needs to be spaced at least n-val partitions apart, as otherwise the writing of a bucket/key onto its primary partitions might not be written onto separate physical nodes.  

However, how far apart should the occurrence of a node in the partition list be spaced?  Clearly it cannot be more than m nodes apart, where m is the number of nodes.  It needs to be at least n-val apart, but n-val is set per bucket in Riak, and so there is no up-front agreement on what that n-val might be.  Also if nodes were spaced apart on the partition list just by n-val, when a single node failed, for some partitions an object may be written to a fallback vnode that is on the same physical node as a primary - and w=2 will not guarantee physical diversity under single node-failure scenarios.

To resolve this, Riak has a configuration parameter of [`target_n_val`](http://docs.basho.com/riak/kv/2.2.3/configuring/reference/) - which represents the target minimum spacing on the partition list for any given physical node.  This by default is set to 4, which allows for physical diversity (subject to sufficient nodes being joined) on n-vals up to 4, and also diversity under single failures for n-vals of up to 3.

Trivially we can achieve this in a partition list by simply allocating vnodes to nodes in sequence:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 ....``

So physical node 1 (n1) is allocated the "first" partition in the ring, n2 the second etc.

### Proximity of vnodes as the partition list wraps

In some cases, the number of nodes will be divisible by the ring_size, and the condition above will be sufficient to ensure diversity.  However, commonly the number of nodes will not divide evenly within the ring-size: for example if we have a ring-size of 32 and 5 physical nodes - the following distribution will be produced:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 |``

The list of allocations wraps around.  So now a bucket/key pair mapping to the 31st partition in the list will be stored on the 31st, 32nd and 1st partitions - which maps to | n1 | n2 | n1 | and so will not guarantee physical diversity.

This wrap-around problem exists only if the remainder is smaller than the target_n_val.  Trivially we can resolve this problem (for non-trivial ring-sizes) by adding further nodes in sequence.  If we assume

`k = target_n_val - (ring_size mod node_count)`

then we can add k nodes in sequence to the tail of the sequence, and then remove each one of the added nodes from one of the 1st to kth sequences at the front of the list.  For the above example this would be:

| n1 | n2 | n3 ... ~~| n4 |~~ ... n5 | n1 | n2 ... ~~| n3 |~~ ... n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | ... ++ ... |n3 | n4 |

Which would give:

``| n1 | n2 | n3 | n5 | n1 | n2 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

### Even distribution of vnodes

A Riak cluster will generally run at the pace of the slowest vnode.  The vnodes should be distributed as to minimise the maximum number of vnodes on any one given node, and ideally have an entirely even distribution of vnodes. So if there are 5 nodes, and a ring-size of 32:

- 3 nodes with 6 vnodes and 2 nodes with 7 vnodes is ideal;

- 4 nodes with 7 vnodes and 1 node with 4 vnodes is sub-optimal - isolating one node with less load will create 'one fast node' that should not have side effects beyond inefficiency;

- 4 nodes with 6 vnodes and 1 node with 8 vnodes is worse still - as it isolates one node with more load than the others, creating 'one slow node', a scenario with known side effects.

An uneven distribution of nodes will be inefficient.  Isolating one node with more load will have side effects beyond inefficiency, especially where this leads to the vnodes on that node frequently going into an overload state;  although it may be unavoidable for one node to have one more partition that all other nodes if the most even balance is desired (for example with a ring-size of 256 and 5 nodes).

### Minimising transfer counts

A simple sequential plan with handling of tail wrapping, as described above may resolve basic issues of distribution of vnodes and spacing between vnodes on a node.  However, it would require significant work on transition.  For example, the equivalent six node cluster would be distributed as such:

``| n1 | n2 | n3 | n5 | n6 | n1 | n2 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 |``

However transitioning from a 5-node cluster to a 6-node cluster would require at least <b>24</b> vnode transfers. As the new vnode only actually requires 5 vnodes to fulfill a balanced role within the cluster - this is a significant volume of excess work.  This problem is exacerbated by the need to schedule transfers so that both the balance of vnodes and the spacing of vnodes is not compromised during the transfer, and so the actual number of transfers required to safely transition may be much higher.

If only the minimum vnodes had been transferred, managing the transfer process if trivial i.e.

to transition from

``| n1 | n2 | n3 | n5 | n1 | n2 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

to

``| n6 | n2 | n3 | n5 | n1 | n6 | n4 | n5 | n1 | n2 | n6 | n4 | n5 | n1 | n2 | n3 | n6 | n5 | n1 | n2 | n3 | n4 | n6 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

would leave each node having either 5 or 6 vnodes, leave the spacing constraints in space, would require only <b>5</b> transfers without any specific scheduling of the transfers required to maintain safety guarantees.  

### Balanced coverage plans

In a six node cluster, with an even distribution of partitions of sequence, and tail wrap-around resolved - we have a ring arrangement like this:

``| n1 | n2 | n3 | n5 | n6 | n1 | n2 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 |``

The coverage plans will hit nodes as such:

Plan 1
- node 1 - 1
- node 2 - 1
- node 3 - 4
- node 4 - 0
- node 5 - 1
- node 6 - 4

Plan 2
- node 1 - 4
- node 2 - 1
- node 3 - 0
- node 4 - 5
- node 5 - 0
- node 6 - 1

Plan 3
- node 1 - 1
- node 2 - 4
- node 3 - 2
- node 4 - 0
- node 5 - 4
- node 6 - 0

The unbalanced coverage plans will lead to unbalanced resource pressure sin the cluster, and in cases of long-running queries issues with multiple 'slow' nodes.

If when sequencing we divide the nodes into blocks of three, with a remainder.  If then we allow the first node in each sequence to roll up and down the three nodes by one place at a time (except for the remainder at the tail of the sequence), we would have an alternative distribution:

``| n1 | n2 | n3 | n5 | n6 | n2 | n1 | n5 | n4 | n6 | n2 | n3 | n1 | n5 | n6 | n4 | n2 | n1 | n3 | n5 | n4 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 |``

The coverage plans will hit nodes as such:

Plan 1
- node 1 - 3
- node 2 - 0
- node 3 - 3
- node 4 - 1
- node 5 - 1
- node 6 - 3

Plan 2
- node 1 - 2
- node 2 - 3
- node 3 - 0
- node 4 - 2
- node 5 - 3
- node 6 - 1

Plan 3
- node 1 - 1
- node 2 - 3
- node 3 - 3
- node 4 - 2
- node 5 - 1
- node 6 - 1

This, in this case will provide for a more even distribution of coverage plans across nodes.  However, this is really only a problem where the number of nodes is a factor of three.  Even in the multiple of three case, although the coverage plans are individually unbalanced, they are collectively balanced; across multiple queries the load is still split across the cluster.  

This should not be a significant issue when scheduling lots of small coverage queries, but may have unpredictable impacts if scheduling larger coverage queries, such as when key-listing.  However, scheduling larger coverage queries on production systems breaches standard best practice guidance in Riak.

### Joining through cluster plans

Within Riak the joining, leaving and transferring of nodes may happen one node at a time, or may be requested in bulk.  So any algorithm must be able to cope with both scenarios.  

The bulk join can of course could always be treated as a series of one-at-a-time changes (although note that in Riak this isn't strictly the case).  

It may also be considered that clusters generally have two states: with less nodes than target_n_val; with a node count greater than or equal to target_n_val.  Also transitioning between these states is a special case - so adding a node or set of nodes to prompt the transition needs to be optimised to create an initially balanced ring, and node additions beyond that transition should be optimised so as not to spoil the balance of the ring.

### Further spacing

``| n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 |``

In this scenario if two nodes were to fail (node 1 and node 2), the fallbacks elected would be split across the nodes so that the proportional activity on the remaining nodes will be:

- node 3 - 27.1%
- node 4 - 20.8%
- node 5 - 14.6%
- node 6 - 12.5%
- node 7 - 12.5%
- node 8 - 12.5%

So with this arrangement, a clearly sub-optimal scenario is created on two node failures - as multiple preflists lost node diversity (i.e. they had fallbacks elected that were not on physically diverse nodes to surviving primaries), and there is a significant bias of load towards one particular node.

So ideally the spacing between partitions for a node should not just be `target_n_val` but further if possible.

## Riak Claim v2 and Upholding Claim properties

There are two non-deprecated claim algorithms in use in Riak today, the default (version 2), and a proposed alternative (version 3).  Both algorithms are "proven" through some property-based tests, which check for randomly chosen scenarios that properties should be upheld.

The properties verified are:
- The target_n_val is upheld, no node owns two partitions within the default target_n_val when the number of nodes is at least the target_n_val;
- No over-claiming, no transition leaves one node with less than ring_size div node_count partitions
- All partitions are assigned;
- No nodes with no claim are assigned any nodes.

However there are two significant issues with this property based testing:
- It assumes one node is added at a time, it does not test for bulk adding of nodes, in particular that bulk-adding which may occur when the cluster is first set-up (e.g. to transition from 1 node to 5 nodes);
- The list of properties is incomplete - no over-claiming does not prove balanced distribution, and there is no testing of balanced coverage plans, optimisation of spacing and minimisation of transition changes.

### An example - Building a 5 node Cluster

The property testing code, tests adding multiple nodes, by adding them one node at a time, and then calling the choose_claim function after each addition.  The actual code works in a subtly different way - this adds all the nodes at once, and then loops over all the added nodes calling the choose_claim function each time.

If we look at a cluster which has a ring-size of 32, and is being expanded from 1 nodes to 5 nodes.  The property testing will add one node at a time, and the call [choose_claim_v2](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L271) every time a node has been added.  Prior to the target_n_val being reached, the result of these iterations is irrelevant.  

When the fourth node is added, the ring_size is now divisible by target_n_val - and then end outcome of that iteration will always be something of the form (regardless of how this is determined):

``| n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 |``

This conclusion will be reached as either the ring has been changed and [RingMeetsTargetN](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L330) will evaluate to true, so that the [closing case clause](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L331) will return the ring.  If not, RingMeetsTargetN will return false, and a [claim_rebalance_n/2](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L337) will be called as the exist from the case clause - and this will always return a ring with the vnodes allocated to nodes in sequence.

The next node add will select nodes to transition to node 5.  It will only select nodes that [do not violate](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L323) the meeting of the target_n_val, so when the final case clause in choose_claim_v2 will be reached with the outcome:

``{RingChanged, EnoughNodes, RingMeetsTargetN} = {true, true, true}``

The modified ring which meets the target_n_val will then be used as the outcome of claim.  However, the input list to select partition is [ordered by the previous owner](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L283).  The algorithm will select non-violating partitions from node 1, then node 2, then node 3 and then node 4 - moving between these lists when the remaining indexes on the list are at 6 partitions (ring_size div node_count).  The algorithm will continue until the wants of node 5 have been satisfied (it wants 6 partitions).  

The effect of this algorithm is that it will take 2 nodes from node 1, 2 from node 2 and 2 nodes from node 3 - and at this stage it will stop taking nodes as the wants have been satisfied (and so no nodes will be taken from node 4).  The end outcome is a partition list like this:

``| n5 | n2 | n3 | n4 | n5 | n2 | n3 | n4 | n1 | n5 | n3 | n4 | n1 | n5 | n3 | n4 | n1 | n2 | n5 | n4 | n1 | n2 | n5 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 |``

This meets the target_n_val, but is unbalanced as the partitions are split between the nodes as follows:

- Node 1 - 6 partitions - 18.75%
- Node 2 - 6 partitions - 18.75%
- Node 3 - 6 partitions - 18.75%
- Node 4 - 8 partitions - 25.00%
- Node 5 - 6 partitions - 18.75%

So node 4 has 33.3% more partitions than any other node.

If as a user of Riak we build a 5 node cluster by adding one node at a time, the cluster will be built as tested in the property test - but this outcome violates a basic desirable property of ring claiming, that it results in an even distribution of nodes, and in particular it does not isolate "one slow node" by focusing additional work in one location.

Although this is the tested scenario, this is not normally how 5 node clusters are built.  Normally, a user would setup five unclustered nodes, and then running a cluster plan to join four of the nodes to the first.  The code path for this approach is subtly different to the tested scenario.

In this scenario, the five nodes will be added to the ring, and choose_claim_v2 will be run four times, once for each joining node.  

This time, on the third loop, which adds the fourth node - the outcome of the algorithm will generally be:

``{RingChanged, EnoughNodes, RingMeetsTargetN} = {true, true, false}``

There is nothing about the picking algorithm used within choose_claim_v2 which will cause a cluster without enough nodes to transition to a cluster which meets `target_n_val` when the target_n_val node is added.

This state will prompt claim_rebalance_n/2 to be the [output of the loop](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L337).  However, unlike in the test scenario the Ring passed to claim_rebalance_n will have all five nodes, not just the first four nodes - as with the cluster plan the ring had all the nodes added in-advance, not just prior to each individual call to choose_claim_v2.  The claim_rebalance_n function looks [at the members](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L501) that have joined the ring (which is all five due to the cluster plan process, not just the four that have been used for iterations through choose_claim_v2).

So claim_rebalance_n will in this case output a simple striping of the partitions across the five nodes <b>with tail violations</b>:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 |``

The function choose_claim_v2 will then be called for a fifth time, for the fifth node.  However, node 5 has in this breaking ring all its wants satisfied - so the algorithm will [not make any ring changes](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L324-L329).  Consequently `RingChangedM = false` at the final case clause, and claim_rebalance_n will be called.  The inputs to claim_rebalance_n are unchanged - and so this will produce the same result <b>with tail violations</b>.

So the end outcome of transitioning from 1 node to 5 nodes in a real cluster will not meet the property of upholding a target_n_val, although the property test will always claim this is upheld.

There is therefore no obvious and direct way to create a 5 node cluster in Riak with a ring that has all the desired properties of a 5-node ring.  More dangerously if using a cluster plan to join all nodes at once, the consequent issue that some data will not be correctly dispersed across physical nodes will not be warned - although [application of best practice process](http://docs.basho.com/riak/kv/2.2.3/setup/upgrading/checklist/#confirming-configuration-with-riaknostic) before go-live should highlight the issue correctly.

Note, that a 5-node cluster is not an unusual exception.  There are multiple potential cases for unexpected outcomes.

## Riak Claim v3 and Upholding Claim properties


## Riak and Proposed Claim Improvements


## Future Thinking - Availability Zones
