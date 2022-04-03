# Accounts

Assets, as described in the [assets section](assets.md), can exists in multiple
places.

Points can live in two places:

 * **A market**. We call this the market’s _capitalization_. The points that are
   held with the market, are used to reward holders of outcome shares when the
   market resolves.
 * **In user’s accounts**. We call this the user’s _balance_ in points.

Outcome shares can live in two similar places:

 * **A market’s market maker pool**. Every market comes with an automated market
   maker (<abbr>AMM</abbr> for short). Users can only trade with the market
   maker, not with eachother. The market maker maintains a pool of outcome
   shares to trade. If the market eventually settles on the actual outcome, the
   shares in the pool will be worth very little (users hold all of the valuable
   shares and made a profit), and the market maker makes a loss.
 * **In user’s accounts**. We call this the user’s _balance_ for the given
   outcome share.
