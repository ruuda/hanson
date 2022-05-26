-- This query is more of an experiment to figure out how to get the data
-- for producing a probability-over-time graph.
select distinct on (
    bin_begin,
    account_id
  )
  date_bin('1 hour', created_at, '2022-01-01T00:00:00+00:00') as bin_begin,
  account_id,
  post_balance as bin_end_balance
from
  account_balance,
  account,
  mutation,
  subtransaction,
  transaction
where
  account_balance.mutation_id = mutation.id
  and account_balance.account_id = account.id
  and mutation.subtransaction_id = subtransaction.id
  and subtransaction.transaction_id = transaction.id
  and account.owner_market_id = 18
  and account.type = 'shares'
order by
  bin_begin,
  account_id,
  -- Per bin, we want the balance of the latest mutation in that bin.
  mutation.id desc;
