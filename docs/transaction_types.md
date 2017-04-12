These are the 17 types of transaction that can be in blocks.

account_new
account_delete
account_repo
account_spend
channel_new
channel_grow
channel_team_close
channel_solo_close
channel_slash
channel_timeout
channel_evidence
channel_repo
oracle_new
oracle_bet
oracle_close
oracle_profit
existence
burn

=== account_new

This creates a new account on the blockchain and gives it some tokens.
This transaction has a hashlock, so it can be connected to a payment on the bitcoin blockchain.

=== account_delete

This deletes an account on the blockchain and sends all of it's money to a different account.
This transaction has a hashlock, so it can be connected to a channel payment.

=== account_recycle

If an account runs out of money anyone can do this transaction to delete the account. The user who deletes the empty account recieves a reward which is smaller than the cost of creating an account.

=== account_spend

This moves tokens or shares from one account to another.
This transaction has a hashlock, so it can be connected to a payment in a channel.

=== channel_new

This creates a new channel on the blockchain.
It needs to be signed by both participants in the channel.
It takes money from both participant's accounts to put into the channel.
This transaction has a hashlock, so it can be connected to a payment, or a channel payment.

=== channel_grow

This adds more money to an existing channel.
This transaction has a hashlock, so it can be connected to a channel payment or a spend transaction.

=== channel_team_close
=== channel_solo_close
=== channel_slash
=== channel_timeout
=== channel_evidence
=== channel_repo
=== oracle_new
=== oracle_bet
=== oracle_close
=== oracle_profit
=== existence

This transaction adds 256 bits of data and an address to the existence tree. This is done to prove that certain data existed at a certain time.
If Channel_slash is done by an untrusted third party, they need to first do a proof of existence of the data, to prove that they deserve the prize.
The virtual machine can verify that data exists in the existence tree.


=== burn

This destroys some coins in a provable way. The destroyed coins are associated with an address, so if you have the private key for that address you can prove that you are the one who destroyed the coins.