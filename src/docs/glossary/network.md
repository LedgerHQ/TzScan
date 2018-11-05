The Network is the list of Nodes running the Tezos Server
Application. From any Node, including the Nodes used by Tz-Scan, only
a partial part of the full network is visible, mostly through the Node
to which the Node could successfully connect.

For every Node, the following Information is displayed:
* Peer: the uniq Identifier of the Node
* Point: the last IP:port used by the Node
* Trusted: ...
* Score: decreases when the Node is suspected of misbehavior.
* State: either Disconnected, Connecting or Accepted
* Total Sent: number of bytes sent to that Node from our Node
* Total Recv: number of bytes received from that Node by our Node
* Inflow: ...
* Outflow: ...
