# Assets

There are two kinds of asset in Hanson:

 * **Points**, the native currency, denoted with symbol ℙ. Points have no
   (intrinsic) value in the real world, they are virtual internet points. If
   Hanson were a real-money prediction market, the currency might have been
   Euros or Bitcoin.
 * **Outcome shares**. Outcome shares are associated with a particular market.
   When the market resolves, outcome shares for the actual outcome will be
   converted into points, while the shares for the other outcomes become
   worthless.

Outcome shares can be created from and destroyed for points, but only
in equal quantities. For example, in a binary market with Y and N outcomes, we
can convert 1&nbsp;point into 1&nbsp;Y + 1&nbsp;N, and vice versa. This is
because when the market resolves, either Y will become worth 1&nbsp;point and N
worthless, or N will become worth 1&nbsp;point and Y worthless, so if you have
1&nbsp;Y and 1&nbsp;N, you effectively have 1&nbsp;point.

Points can only be created by the system. The system creates points for two
reasons:

 * **Income**. Every user gets some points every day, to participate in the
   markets and create new ones. Think of it as pocket money, an allowance, or a
   basic income.
 * **Subsidies**. Good markets can be subsidized by increasing the payout of its
   outcome shares.

The system can destroy points in one occasion:

 * **Market resolution**. The automated market maker of a market holds a pool
   of outcome shares, which may become valuable when the market resolves. Since
   no user held these outcome shares, the points are removed by the system.
