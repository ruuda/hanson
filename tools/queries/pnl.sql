with credits as (
  select
    amount
  from
    mutations,
    subtransactions,
    accounts as account_user,
    accounts as account_market
  where
    subtransactions.type = 'exchange_create_shares'
    and mutations.subtransaction_id = subtransactions.id
    and mutations.credit_account_id = account_user.id
    and mutations.debit_account_id = account_market.id
    and account_user.owner_user_id = 5
    and account_market.owner_market_id = 21
),
debits as (
  select
    amount
  from
    mutations,
    subtransactions,
    accounts as account_user,
    accounts as account_market
  where
    subtransactions.type = 'exchange_destroy_shares'
    and mutations.subtransaction_id = subtransactions.id
    and mutations.debit_account_id = account_user.id
    and mutations.credit_account_id = account_market.id
    and account_user.owner_user_id = 5
    and account_market.owner_market_id = 21
)
select
  (select sum(amount) from credits) as credit_amount,
  (select sum(amount) from debits) as debit_amount;
