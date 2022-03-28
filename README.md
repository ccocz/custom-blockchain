# Effortless custom blockchain

## Merkle tree
`buildTree :: Hashable a => [a] -> Tree a` can be used to build a hash tree containing all the elements of the list as its arguments,
e.g.,

```haskell
> putStr $ drawTree $ buildTree "doge"
0xaef9224b -
  0x64009ddb -
    0x00000064 'd'
    0x0000006f 'o'
  0x6700a28a -
    0x00000067 'g'
    0x00000065 'e'
```

## Proofs
Proof that an element belongs to a tree with a specific root is defined as the path (from the root to the leaf), 
each element of which contains information which descendant contains the element of interest
and the hash of the second descendant.
```haskell
> buildProof 'o' $ buildTree "sol"
Just (MerkleProof 'o' <0x6c00aa70>0x00000073)
```

## Blockchain
Blockchain is simplified and based on "Proof of Work" scheme.
```haskell
mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txs
```
creates a block that is a valid extension of a chain terminated with `parentHash` and containing `txs` transactions.

Block consists of a header and a list of transactions:
```haskell
data Block = Block { 
  blockHdr :: BlockHeader,
  blockTxs :: [Transaction]
}

data BlockHeader = BlockHeader {
  parent :: Hash, 
  coinbase :: Transaction, 
  txroot :: Hash -- root of the Merkle tree
  nonce :: Hash
}
```

where `nonce` is proof of work - value such that the header hash meets a given condition.

## Tests
Inline tests beginning with `>>>` sequence are placed in modules and can be tested with `doctest`, e.g.,
```bash
$ doctest Doctests.hs
Examples: 26  Tried: 26  Errors: 0  Failures: 0
```