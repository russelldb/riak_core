# The Ring and Claim

## Summary

It is common when presenting Riak to throw in a picture of the ring and assume everyone understands what this means.

![](ibm_ring.png "IBM Presents the Ring")

However, the ring, and in particular the process by which nodes claim from the ring isn't necessarily obvious to everyone.  Perhaps sometimes even the speaker doesn't really understand what this means.  

This is an attempt to explain the problems associated with claiming from a ring, and examine the current implementations of claim within Riak, and also investigate some potential areas for improvement.

## Overview of The Ring - the Basics

At design time of a Riak cluster, it is necessary to set the [size of the ring](http://docs.basho.com/riak/kv/2.2.3/configuring/basic/#ring-size) to a power of two.  Getting this right at the start is important, as although there exists the potential to resize a ring post go-live, it is difficult to do in a risk-free way under production load.  Currently, ring re-sizing is not a solved problem.  Guidance on how to size a ring in advance is [vague](http://docs.basho.com/riak/kv/2.2.3/setup/planning/cluster-capacity/#ring-size-number-of-partitions).  Currently, correctly setting the size of a ring to avoid resizing is not a solved problem.

The size of the ring simply defines the number of separate (and equal) partitions of the hash space there will be.  that is to say each Bucket/Key combination will be hashed before storage, and hashed before fetching - and the outcome of that consistent hashing algorithm will determine the partition to which that Bucket/Key pair belongs.  Each partition has an ID, which is the floor hash value for that partition - to belong to a partition a Bucket/Key pair must hash to a value which is greater than the floor, and less than or equal to the floor of the next partition.  The "first" partition is ID 0, but this is only the first in the sense it has the lowest Partition ID - it has no special role in the ring.

Within a Riak cluster there are individual databases known as vnodes, where each vnode is associated with a single Partition ID.  Every bucket within Riak has an n-val which indicates how many vnodes each value should normally be stored in, and retrieved from.  

When a key is to be stored or retrieved the Bucket/Key is hashed and then the operation will be performed against the vnode  with the Partition ID which the Key hashes to, plus the next n-1 vnodes in numerical order of Partition ID.  If one of these vnodes is known to be unavailable at commencement of the operation (Riak will gossip the status of vnodes around the cluster), then the next vnode in numerical order will also be used as a fallback - and if more vnodes expected to be used are unavailable, more fallbacks will be used.  

If the database operation doesn't know the keys it is interested in, for example in a secondary index query, it must ask every nth vnode - where n is the n-val for the bucket related to that query.  If there is a n-val of three, that means there are three different coverage plans in a healthy cluster that could be used for the query, with the offset being chosen at random to determine which plan will be used.  If a vnode is unavailable for a coverage query, using a fallback node is not a desirable answer, as the coverage query is dependent on each vnode having a complete set of answers.  So the coverage plan must assess the partitions which are not covered by the plan, and find other vnodes that can fill the role of the missing vnodes.  

For most vnodes in a coverage plan, the coverage plan is interested in all the answers present in that vnode, but where the coverage plan is looking for a vnode to fill a gap in the coverage, the query is passed with a partition filter so that only results in specific partitions are fetched - avoiding unnecessary duplication of results.  This same filter is used for partitions at the tail of the coverage plan, which would otherwise return results that have already been found from the front of the plan (where the ring-size is not divisible by three).

## Overview of the Ring - the Claiming problem

If we have a ring - which is really a list of partitions of a key space, which are mapped in turn to vnodes (individual key-value stores): the key question is how do we distribute these vnodes around the physical cluster.  Riak has no specifically assigned roles (i.e. there is no master controlling node), so distribution of vnodes is managed by a process of nodes claiming the number of vnodes they want, and a consensus being reached within the cluster what the up-to-date set of claims are.  

There are some properties of this claiming process which are required, and are listed here in decreasing order of obviousness:

- vnodes within n partitions of each other should not be mapped to the same physical nodes;
- vnodes at the tail of the partition list, should not be mapped to the same physical nodes as vnodes at the front of the partition list (i.e. the above property must hold as the tail wraps around to the start);
- there should be an even distribution of vnodes across the physical nodes;
- transitions from one cluster state to another should minimise the number of transfers required;
- all coverage plans should be evenly spread across the physical nodes (so that expensive queries don't cause a subset of slow nodes);
- the above properties should be upheld if nodes are added one-by-one, or joined in bulk;
- vnodes should be generally be spaced as a far as possible, not just minimally.

Each of these properties will be examined in turn, and the explanation of those properties will hint at both how Riak may fail to meet some of these properties, and how that failure could be resolved.  The specific problems in Riak and the proposed solution will be explained in a later section.

### Proximity of vnodes

The mapping of vnodes to nodes needs to be spaced at least n-val partitions apart, as otherwise the writing of a bucket/key onto its primary partitions might not be written onto separate physical nodes.  

However, how far apart should the occurrence of a node in the partition list be spaced?  Clearly it cannot be more than m nodes apart, where m is the number of nodes.  It needs to be at least n-val apart, but n-val is set per bucket in Riak, and so there is no up-front agreement on what that n-val might be.  Also if nodes were spaced apart on the partition list just by n-val, when a single node failed, for some partitions an object may be written to a fallback vnode that is on the same physical node as a primary - and w=2 will not guarantee physical diversity under single no failure scenarios.

To resolve this, Riak has a configuration parameter of [`target_n_val`](http://docs.basho.com/riak/kv/2.2.3/configuring/reference/) - which represents the target minimum spacing on the partition list for any given physical node.  This by default is set to 4, which allows for physical diversity (subject to sufficient nodes being joined) on n-vals up to 4, and also diversity under single failures for n-vals of up to 3.

Trivially we can achieve this in a partition list by simply allocating vnodes to nodes in sequence:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 ....``

### Proximity of vnodes as the partition list wraps

In some cases, the number of nodes will be divisible by the ring_size, and the condition above will be sufficient to ensure diversity.  However, commonly the number of nodes will not divide evenly within the ring-size: for example if we have a ring-size of 32 and 5 physical nodes - the following distribution will be produced:

``| n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 |``

So now a bucket/key pair mapping to the 31st partition in the list will be stored on | n1 | n2 | n1 | and so not guarantee physical diversity.

This wrap-around problem exists only if the remainder is smaller than the target_n_val.  Trivially we can resolve this problem (for non-trivial ring-sizes) by adding further nodes in sequence.  If we assume

`k = target_n_val - (ring_size mod node_count)`

then we can add k nodes in sequence to the tail of the sequence, and then remove each one of the added nodes from one of the 1st to kth sequences at the front of the list.  For the above example this would be:

| n1 | n2 | n3 ... ~~| n4 |~~ ... n5 | n1 | n2 ... ~~| n3 |~~ ... n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | ... ++ ... |n3 | n4 |

Which would give:

``| n1 | n2 | n3 | n5 | n1 | n2 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

### Even distribution of vnodes

A Riak cluster will generally run at the pace of the slowest vnode.  The vnodes should be distributed as to minimise the maximum number of vnodes on any one given node, and ideally have an entirely even distribution of vnodes. So if there are 5 nodes, and a ring-size of 32:

- 3 nodes with 6 vnodes and 2 nodes with 7 vnodes is ideal;
- 4 nodes with 7 vnodes and 1 node with 4 vnodes is sub-optimal;
- 4 nodes with 6 vnodes and 1 node with 8 vnodes is worse still.

### Minimising transfer counts

A simple sequential plan with handling of tail wrapping, as described above may resolve basic issues of distribution of vnodes and spacing between vnodes on a node.  However, it would require significant work on transition.  For example, the equivalent six node cluster would be distributed as such:

``| n1 | n2 | n3 | n5 | n6 | n1 | n2 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 | n5 | n6 | n1 | n2 | n3 | n4 |``

However transitioning from a 5-node cluster to a 6-node cluster would require <b>24</b> vnode transfers. As the new vnode only actually requires 5 vnodes to fulfill a balanced role within the cluster - this is a significant volume of excess work.  This problem is exacerbated by the need to schedule transfers so that both the balance of vnodes and the spacing of vnodes is not compromised during the transfer.

If only the minimum vnodes had been transferred, managing the transfer process if trivial i.e.

to transition from

``| n1 | n2 | n3 | n5 | n1 | n2 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

to

``| n6 | n2 | n3 | n5 | n1 | n6 | n4 | n5 | n1 | n2 | n6 | n4 | n5 | n1 | n2 | n3 | n6 | n5 | n1 | n2 | n3 | n4 | n6 | n1 | n2 | n3 | n4 | n5 | n1 | n2 | n3 | n4 |``

would leave each node having either 5 or 6 vnodes, leave the spacing constraints in space, would require only <b>5</b> transfers without any specific scheduling of the transfers to maintain safety guarantees.

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

This, in this case will provide for a more even distribution of coverage plans across nodes.  However, this is really only a problem where the number of nodes is a factor of three.  Even in the multiple of three case, although the coverage plans are individually unbalanced, they are collectively balanced - across multiple queries the load is still split across the cluster.  This perhaps is not necessarily a huge issue.

### Joining through cluster plans

Within Riak the joining, leaving and transferring of  nodes may happen one node at a time, or may be requested in bulk.  

### Further spacing

``| n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n1 | n2 | n3 | n4 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 | n5 | n6 | n7 | n8 ``

In this scenario if two nodes were to fail (node 1 and node 2), the fallbacks elected would be split across the nodes so that the proportional activity on the remaining nodes will be:

- node 3 - 27.1%
- node 4 - 20.1%
- node 5 - 14.6%
- node 6 - 12.5%
- node 7 - 12.5%
- node 8 - 12.5%

So with this arrangement, a clearly sub-optimal scenario is created on two node failures - as multiple preflists lost node diversity (i.e. they had fallbacks elected that were not on physically diverse nodes to surviving primaries), and there is a significant bias of load towards one particular node.

So ideally the spacing between partitions for a node should not just be `target_n_val` but further if possible.

## Riak and Upholding Claim properties


## Riak and Proposed Claim Improvements
