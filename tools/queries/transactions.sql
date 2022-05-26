select
  created_at,
  account_id,
  credit_account_id,
  debit_account_id,
  mutation_id,
  amount,
  post_balance,
  subtransaction.type as subtx_type,
  transaction.type as tx_type,
  subtransaction_id,
  transaction_id,
  creator_user_id
from
  account_balance,
  mutation,
  subtransaction,
  transaction
where
  account_balance.mutation_id = mutation.id
  and mutation.subtransaction_id = subtransaction.id
  and subtransaction.transaction_id = transaction.id;
