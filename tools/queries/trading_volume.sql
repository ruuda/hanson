select
  amount,
  *
from
  mutations,
  subtransactions,
  transactions,
  accounts
where
  subtransactions.type in ('exchange_create_shares', 'exchange_destroy_shares')
  and subtransactions.transaction_id = transactions.id
  and mutations.subtransaction_id = subtransactions.id
  and (mutations.credit_account_id = accounts.id or mutations.debit_account_id = accounts.id)
  --and accounts.owner_market_id = 10
  and accounts.owner_market_id is not null
  and transactions.created_at > now() - '10h'::interval;
