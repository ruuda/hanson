# Transactions

[Assets](assets.md) in Hanson can move from one [account](accounts.md) to
another through _transactions_. There are several types of transactions:

 * **Income**, when the system creates new points to increase a user’s balance.
 * **Exchange**, when points are converted into outcome shares, or vice versa.
 * **Trade**, when a user trades outcome shares against an <abbr>AMM</abbr>.
   The <abbr>AMM</abbr> only trades in outcome shares, but if a user doesn’t
   have the right outcome shares to pay with, then they need to perform an
   exchange transaction before the trade. The system will do this automatically.
   Similarly, after a trade, a user might end up with a complete set of outcome
   shares. The system automatically exchanges those back to points. This ensures
   that only users who have a position in the market benefit from subsidies.
   Exchange is only possible for markets that are not yet resolved.
 * **Subsidize**, when the system creates new points to increase a market’s
   capitalization, without creating additional outcome shares. This affects the
   exchange rate for that market by making the outcome shares more valuable.
 * **Resolve**, when a market resolves, and the value of the outcome shares
   becomes known. In this transaction, the points that were allocated to the
   market’s capitalization are distributed over the users who hold outcome
   shares in the realized outcome. The opposite happens in case of an unresolve.

A transaction consists of one or more _mutations_. A mutation is a change in
balance for a particular account.
