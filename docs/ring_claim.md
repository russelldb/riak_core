# The Ring and Claim

## Summary

It is common when presenting Riak to throw in a picture of the ring and assume everyone understands what this means.

![](ibm_ring.png "IBM Presents the Ring")

However, the ring, and in particular the process by which nodes claim from the ring isn't necessarily obvious to everyone.  Perhaps sometimes even the speaker doesn't really understand what it all means.  

This is an attempt to explain the problems associated with claiming from a ring, examine the current implementations of claim within Riak and some issues with it, and then discuss areas for improvement.

## Overview of The Ring - the Basics

At design time of a Riak cluster, it is necessary to set the [size of the ring](http://docs.basho.com/riak/kv/2.2.3/configuring/basic/#ring-size) to a power of two.  Getting this right at the start is important, as although there exists the potential to resize a ring post go-live, it is difficult to do in a risk-free way under production load.  Currently, ring re-sizing is not a solved problem.  Guidance on how to size a ring in advance is [vague](http://docs.basho.com/riak/kv/2.2.3/setup/planning/cluster-capacity/#ring-size-number-of-partitions).  Currently, correctly setting the size of a ring to avoid resizing is not a solved problem.

The size of the ring simply defines the number of separate (and equal) partitions of the hash space there will be.  Each Bucket/Key combination will be hashed before storage, and hashed before fetching, and the outcome of that consistent hashing algorithm will determine the partition to which that Bucket/Key pair belongs.  Each partition has an ID, which is the floor hash value for that partition: to belong to a partition a Bucket/Key pair must hash to a value which is greater than the floor, and less than or equal to the floor of the next partition.  The "first" partition is ID 0, but this is only the first in the sense it has the lowest Partition ID - it has no special role in the ring.

Within a Riak cluster there are individual databases known as vnodes, where each vnode is associated with a single Partition ID.  The more partitions, the easier it is to spread load smoothly throughout larger and larger clusters.  The fewer partitions, the lower the overhead of running all the vnodes necessary to support each partition.  As the node count increases, so does the optimal number of vnodes.

Every bucket within Riak has an n-val which indicates how many vnodes each value should normally be stored in, and retrieved from.  When a key is to be stored or retrieved the Bucket/Key is hashed and then the operation will be performed against the vnode  with the Partition ID which the Key hashes to, plus the next n-1 vnodes in numerical order of Partition ID.  If one of these vnodes is known to be unavailable at commencement of the operation (Riak will gossip the status of vnodes around the cluster), then the next vnode in numerical order will also be used as a fallback (i.e. the node n partitions along from the one the key hashed to).  If more vnodes expected to be used are unavailable, more fallbacks will be used.  

If the database operation doesn't know the keys it is interested in, for example in a secondary index query, it must ask every nth vnode - where n is the n-val for the bucket related to that query.  If there is a n-val of three, that means there are three different coverage plans in a healthy cluster that could be used for the query, with the offset being chosen at random to determine which plan will be used.  To be explicit, there will be one coverage plan based on the 1st partition, the 4th partition, the 7th partition etc - and two more coverage plans where the partitions used are right shifted by 1 and 2 partitions respectively.

If a vnode is unavailable for a coverage query, using a fallback node is not a desirable answer.   Coverage queries are run with r=1 (only one vnode is checked for every part of the keyspace), so fallback nodes are unsafe as they only contain the data since the cluster change event occurred for some of those partitions.  So the coverage planner must assess the partitions which are not covered by the plan, and find other vnodes that can fill the role of the missing vnodes.  

TODO: THIS IS TOO MUCH TOO EARLY

For most vnodes in a coverage plan, the coverage plan is interested in all the answers present in that vnode, but where the coverage plan is looking for a vnode to fill a gap in the coverage, the query is passed with a partition filter so that only results in specific partitions are fetched.  Using the partition filter avoids unnecessary duplication of results.  This same filter is used for partitions at the tail of the coverage plan, which would otherwise return results that have already been found from the front of the plan (as the ring-size will not be divisible by three).

## Overview of the Ring - the Claiming problem

If we have a ring - which is really a list of partitions of a key space, which are mapped in turn to vnodes (individual key-value stores): the key question is how do we distribute these vnodes around the physical cluster.  

Riak has no specifically assigned roles (i.e. there is no master controlling node), so in theory distribution of vnodes can be managed by a process of nodes claiming the number of vnodes they want, and a consensus being reached within the cluster what the up-to-date set of claims are.  Different nodes can propose changes to the ring, and detection of conflicts between different proposals is managed using vector clocks.  In practice, such changes to claim are made through administrative action using cluster plans, and the node on which the cluster plan is made will make claims on the ring on behalf of the nodes involved in the change.

There are some properties of this claiming process which are required, and are listed here in decreasing order of obviousness:

- vnodes within n partitions of each other should not be mapped to the same physical nodes (so that writes in a healthy cluster are always safely distributed across different nodes);

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

This wrap-around problem exists only if the remainder is smaller than the target_n_val.  This problem could be resolved (for non-trivial ring-sizes) by adding further nodes in sequence.  If we assume

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

An uneven distribution of vnodes will be inefficient.  Isolating one node with more load will have side effects beyond inefficiency, especially where this leads to the vnodes on that node frequently going into an overload state;  although it may be unavoidable for one node to have one more partition that all other nodes if the most even balance is desired (for example with a ring-size of 256 and 5 nodes).

### Minimising transfer counts

A simple sequential plan with handling of tail wrapping, as described above may resolve basic issues of distribution of vnodes and spacing between vnodes on a node.  However, it would require significant work on transition.  For example, the equivalent six node cluster would be distributed as such:

``| n1 | n2 | n3 | n5 | n6 | n1 | n2 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 |``

However transitioning from a 5-node cluster to a 6-node cluster would require at least <b>24</b> vnode transfers. As the new node only actually requires 5 vnodes to fulfill a balanced role within the cluster - this is a significant volume of excess work.  This problem is exacerbated by the need to schedule transfers so that both the balance of vnodes and the spacing of vnodes is not compromised during the transfer, and so the actual number of transfers required to safely transition may be much higher.

If only the minimum vnodes had been transferred, managing the transfer process is trivial i.e.

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

The unbalanced coverage plans will lead to unbalanced resource pressures in the cluster, and in cases of long-running queries issues with multiple 'slow' nodes.

If when sequencing we divide the nodes into blocks of three, with a remainder, and then we allow the first node in each sequence to roll up and down the three nodes by one place at a time (except for the remainder at the tail of the sequence), we would have an alternative distribution:

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

This will provide for a more even distribution of coverage plans across nodes.  However, this is really only a problem where the number of nodes is a factor of three.  Even in the multiple of three case, although the coverage plans are individually unbalanced, they are collectively balanced; across multiple queries the load is still split across the cluster.  

This should not be a significant issue when scheduling lots of small coverage queries, but may have unpredictable impacts if scheduling larger coverage queries, such as when key-listing.  However, scheduling larger coverage queries on production systems breaches standard best practice guidance in Riak.

### Joining through cluster plans

Within Riak the joining, leaving and transferring of nodes may happen one node at a time, or may be requested in bulk.  So any algorithm must be able to cope with both scenarios.  

The bulk join could always be treated as a series of one-at-a-time changes (although note that in Riak this isn't strictly the case).  

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

With this arrangement, a clearly sub-optimal scenario is created on two node failures - as multiple preflists lost node diversity (i.e. they had fallbacks elected that were not on physically diverse nodes to surviving primaries), and there is a significant bias of load towards one particular node.

Ideally the spacing between partitions for a node should not just be `target_n_val` but further if possible.

## Riak Claim v2 and Upholding Claim properties

There are two non-deprecated claim algorithms in use in Riak today, the default (version 2), and a proposed alternative (version 3).  Both algorithms are "proven" through some property-based tests, which check for randomly chosen scenarios that properties should be upheld.

The properties verified are:
- The target_n_val is upheld, no node owns two partitions within the default target_n_val when the number of nodes is at least the target_n_val;
- No over-claiming, no transition leaves one node with less than ring_size div node_count partitions
- All partitions are assigned;
- Nodes with no claim are not assigned any vnodes.

However there are two significant issues with this property based testing:
- It assumes one node is added at a time, it does not test for bulk adding of nodes, in particular that bulk-adding which may occur when the cluster is first set-up (e.g. to transition from 1 node to 5 nodes);
- The list of properties is incomplete - no over-claiming does not prove balanced distribution, and there is no testing of balanced coverage plans, optimisation of spacing and minimisation of transition changes.

### An example - Building a 5 node Cluster

The ring is an Erlang record which can be passed around, until it is finalised and then gossiped to seek agreement for the completed transition.  A new node becomes a member of the ring when it is added to the record, but is only assigned partitions in the record when the ring and the node are passed to [choose_claim_v2](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L271).  

If we look at a cluster which has a ring-size of 32, and is being expanded from 1 nodes to 5 nodes.  The property testing will add one node at a time to the ring record, and the call [choose_claim_v2](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L271) every time a node has been added.  

Prior to the target_n_val being reached, the result of these iterations is irrelevant.  When the fourth node is added, the ring_size is now divisible by target_n_val - and then end outcome of that iteration will always be something of the form (regardless of how this is determined):

``| n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 |``

This conclusion will be reached as either the ring has been changed and [RingMeetsTargetN](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L330) will evaluate to true, so that the [closing case clause](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L331) will return the ring.  If not, RingMeetsTargetN will return false, and a [claim_rebalance_n/2](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L337) will be called as the exit from the case clause - and this will always return a ring with the vnodes allocated to nodes in sequence.

The next node add will select vnodes to transition to node 5.  It will only select nodes that [do not violate](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L323) the meeting of the target_n_val, so when the final case clause in choose_claim_v2 will be reached with the outcome:

``{RingChanged, EnoughNodes, RingMeetsTargetN} = {true, true, true}``

The modified ring which meets the target_n_val will then be used as the outcome of claim.  However, the input list to select partition is [ordered by the previous owner](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L283).  The algorithm will select non-violating partitions from node 1, then node 2, then node 3 and then node 4.  The algorithm will move on from one node's list when the remaining indexes on the list are at 6 partitions (ring_size div node_count) - so giving up another node would make the donating node's contribution to the ring deficient for providing overall balance.  The algorithm will continue until the wants of node 5 have been satisfied (it wants 6 partitions).  

The effect of this algorithm is that it will take 2 nodes from node 1, 2 from node 2 and 2 nodes from node 3 - and at this stage it will stop taking nodes as the wants have been satisfied (and so no nodes will be taken from node 4).  The end outcome is a partition list like this:

``| n5 | n2 | n3 | n4 | n5 | n2 | n3 | n4 | n1 | n5 | n3 | n4 | n1 | n5 | n3 | n4 | n1 | n2 | n5 | n4 | n1 | n2 | n5 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 |``

This meets the target_n_val, but is unbalanced as the partitions are split between the nodes as follows:

- Node 1 - 6 partitions - 18.75%
- Node 2 - 6 partitions - 18.75%
- Node 3 - 6 partitions - 18.75%
- Node 4 - 8 partitions - 25.00%
- Node 5 - 6 partitions - 18.75%

So node 4 has 33.3% more partitions than any other node.

If as a user of Riak we build a 5 node cluster by adding one node at a time, the cluster will be built as tested in the property test - but this outcome violates a basic desirable property of ring-claiming, that it results in an even distribution of nodes.  The outcome isolates "one slow node" by focusing relatively-significant additional work in one location.

Although this is the tested scenario, this is not normally how 5 node clusters are built.  Normally, a user would setup five unclustered nodes, and then running a cluster plan to join four of the nodes to the first.  

The property testing code tests adding multiple nodes by adding them one node at a time to the ring, and then calling the choose_claim function after each addition, the actual code called during cluster planning works in a subtly different way.  The actual process adds all the nodes to be added as members to the ring first, and then the code loops over all the added nodes calling the choose_claim function each time.  So both the test code and real code loop over choose_claim for each added vnode, but in the test code there are no to-be added members on the ring when choose_claim is called each time, whereas in cluster planning all new nodes are added to the ring before the first call of choose_claim.  

There is a subtle difference between the process of adding one node in one plan and adding multiple nodes in one plan, and the property-based testing only confirms safe operation in the first case.

In the real-world cluster-planning scenario, the five nodes will be added to the ring, and choose_claim_v2 will be run four times, once for each joining node.  On the third loop, which adds the fourth node - the outcome of the algorithm will generally be:

``{RingChanged, EnoughNodes, RingMeetsTargetN} = {true, true, false}``

There is nothing about the picking algorithm used within choose_claim_v2 which will cause a cluster without enough nodes to transition to a cluster which meets `target_n_val` when the target_n_val'th node is added.

This state will prompt claim_rebalance_n/2 to be the [output of the loop](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L337).  However, unlike the test scenario, the ring record passed to claim_rebalance_n will have all five nodes, not just the first four nodes - as with the cluster plan the ring had all the nodes added in-advance, not just prior to each individual call to choose_claim_v2.  The claim_rebalance_n function looks [at the members](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L501) that have joined the ring (which is all five due to the cluster plan process, not just the four that have been used for iterations through choose_claim_v2).

So claim_rebalance_n will in this case output a simple striping of the partitions across the five nodes <b>with tail violations</b>:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 |``

The function choose_claim_v2 will then be called for a fifth time, for the fifth node.  However, node 5 has in this breaking ring all its wants satisfied - so the algorithm will [not make any ring changes](https://github.com/martinsumner/riak_core/blob/develop/src/riak_core_claim.erl#L324-L329).  Consequently `RingChangedM = false` at the final case clause, and claim_rebalance_n will be called.  The inputs to claim_rebalance_n are unchanged from the first call, and so this will produce the same result <b>with tail violations</b>.  

So the end outcome of transitioning from 1 node to 5 nodes in a real cluster will not meet the property of upholding a target_n_val, although the property-based testing will always claim this is upheld.  Further, even when an additional node is added to the cluster, the algorithm will only resolve once of these tail violations.  Only the addition of a seventh node will lead to a cluster upholding the target_n_val.

There is therefore no obvious and direct way to create a 5-node cluster in Riak with a 32-partition ring that has all the desired properties of a 5-node ring.  More dangerously if using a cluster plan to join all nodes at once, the consequent issue that some data will not be correctly dispersed across physical nodes will not be warned; although [application of best practice process](http://docs.basho.com/riak/kv/2.2.3/setup/upgrading/checklist/#confirming-configuration-with-riaknostic) before go-live should highlight the issue correctly.

Note, that a 5-node cluster is not a lonely exception.  There are similar issues starting a six node cluster with a ring-size of 128.  Starting a 5 node cluster with a ring-size of 128 through a cluster plan will not have any preflists onto split across nodes, but it will not meet the target_n_val - which is a more difficult issue to spot without manually drawing out the ring.

### Claim v2 - Evaluation

Evaluating the Claim v2 algorithm reveals the following weaknesses:

- Property-based testing doesn't correctly test the safety clusters formed through cluster plans;

- The algorithm (and the test) don't check correctly for a truly even distribution of vnodes across nodes;

- The algorithm doesn't resolve tail violations when falling back to sequenced allocation of partitions;

- When adding one node at a time, the algorithm will not resolve all violations in the previous ring, even if such violations are resolvable.

- The algorithm doesn't consider the optimality of the outcome (could vnodes be further apart than target_n_val, could coverage query load be better balanced).

The Claimv2 algorithm does have the following positives:

- Adding one node at a time will never break the target_n_val;

- Once a cluster ring configuration supports the target_n_val, adding further nodes will never introduce target_n_val violations;

- The algorithm will produce a minimal number of transfers in the case of node addition.

## Riak Claim v3 and Upholding Claim properties

Riak has a v3 claim algorithm, which is not currently enabled by default.  It can however be enabled within a cluster, should issues be discovered with v2 claim.

The algorithm and its purpose is described briefly in the code:

> Claim V3 - unlike the v1/v2 algorithms, v3 treats claim as an optimization problem. In it's current form it creates a number of possible claim plans and evaluates them for violations, balance and diversity, choosing the 'best' plan.
>
> Violations are a count of how many partitions owned by the same node are within target-n of one another. Lower is better, 0 is desired if at all possible.
>
> Balance is a measure of the number of partitions owned versus the number of partitions wanted.  Want is supplied to the algorithm by the caller as a list of node/counts.  The score for deviation is the RMS of the difference between what the node wanted and what it has.  Lower is better, 0 if all wants are met.
>
> Diversity measures how often nodes are close to one another in the preference list.  The more diverse (spread of distances apart), the more evenly the responsibility for a failed node is spread across the cluster.  Diversity is calculated by working out the count of each distance for each node pair (currently distances are limited up to target N) and computing the RMS on that.  Lower diversity score is better, 0 if nodes are perfectly diverse.

The algorithm calculates then end distribution of vnodes across nodes (by count) up-front, without reference to the current distribution of vnodes.  These "wants" will produce a cluster with a balanced distribution of vnodes.  For example if he outcome is a 5-node cluster with a ring-size of 32, the algorithm will allocate a target count to each node of 6 vnodes to 3 nodes, and 7 vnodes to 2 nodes, before beginning the process of proposing transfers.

The algorithm then examines the current ring, and looks for violations and overloads.  Violations are partitions in breach of target_n_val, with both elements of the pair in violation included in the list of violations.  Overloads are all the partitions belong to all of the nodes that own more partitions than their target ownership count.  In most cases when adding nodes to a relatively small cluster, this will be all the partitions.

Two rounds of "takes" are attempted.  Firstly, for each partition in violation, the partition is offered to a randomly-selected node which is capable (i.e. the node has spare capacity under its target count, and has no partition within target_n_val of the offered partition) of taking that violating partition.  This will create a new version of the ring.

The overloads are then calculated based on the view of the ring after resolving violations.  The same random take process is used then to distribute the overload indexes until no node is a "taker" - i.e. no node has spare capacity, or there are no spare indexes left for any nodes to take.

This process of randomly distributing the violating and overloaded partitions to nodes that have capacity and would not cause a breach, will output a plan for a new ownership structure.  The claim_v3 algorithm will run multiple (default 100) iterations of this generating a new potential plan each time.  The plans are then scored on the distances between partitions across the nodes - trying to maximise the spacing of indexes across all nodes.  the best plan is chosen as the result.

The violation score of the best plan is checked before returning the proposal.  If the violation score is 0, that is the proposed plan.  If the best plan has a non-zero violation score, then a new ring arrangement is calculated using the claim_diversify/3, with the new ring arrangement ignoring all previous allocations.  The claim_diversify function supports an algorithm that tries to assign each partition one at a time to a node that will not break the target_n_val.  If there is more than one eligible node a scoring algorithm is used to choose the next one.

The scoring is mysterious in its actual aim.  If we have a series of allocations all within the target_n_val it tends to prefer the most 'jumbled' of these scores (lower scores are considered better):

```
score([a,b,c,d,e,f,a,b,c,d,e,f]), 4).
76.79999999999986

score([a,c,b,d,e,f,a,c,b,d,e,f]), 4).
76.79999999999988

score([a,c,b,d,e,f,a,b,c,d,e,f]), 4).
46.79999999999991

score([a,c,b,d,f,e,a,b,c,d,e,f]), 4).
40.799999999999955

score([a,c,b,d,f,e,b,a,c,d,e,f]), 4).
38.59166666666669

score([f,c,b,d,f,e,b,a,c,d,e,a]), 4).
32.125
```
However, it may also prefer sequences that have risks of breaches on dual node failures, over those sequences that don't have such issues:

```
ScoreFun([a, b, c, d, e, f, g, h, a, b, c, d, e, f, g, h], 4).
109.71428571428542

ScoreFun([a, b, c, d, a, b, c, d, e, f, g, h, e, f, g, h], 4).
66.0
```

This scoring anomaly may be related to a constraint to continue only diversity within the target_n_val distance.  This reasoning behind this restriction is unclear.

The claim_diversify algorithm doesn't necessarily prevent preventable target_n_val violations.  It tries to avoid them, but does no back-tracking to resolve them when they occur, and simply logs at a debug level to notify of the breach.

Ultimately the user who initiated the plan will be presented with the final proposal, unaware as to the reasons as to why it is considered to be the best proposal, or even if it meets the requested conditions (e.g. target_n_val).  The user has a choice, of either accepting the proposal or re-rolling the dice and request another proposal.  If the next proposal is considered by the user to be "worse", they cannot revert to the previous proposal, they must keep requesting new proposals until they hit again a proposal they consider to be optimal.

### Claim v3 - Evaluation



## Riak and Proposed Claim Improvements


## Future Thinking

### Physical promises


### Availability Zones
