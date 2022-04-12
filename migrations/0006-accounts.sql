-- migrate:up

CREATE DOMAIN points AS NUMERIC(14, 2); -- 14 digits total, two behind the decimal point.

CREATE TYPE account_type AS ENUM ('points', 'shares');

CREATE TABLE account
  ( id              BIGINT       NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , type            account_type NOT NULL
  , outcome_id      BIGINT       NULL REFERENCES "outcome" (id)
  , owner_user_id   BIGINT       NULL REFERENCES "user" (id)
  , owner_market_id BIGINT       NULL REFERENCES "market" (id)
  , CONSTRAINT outcome_iff_outcome_share CHECK
    (
      CASE type
        WHEN 'points' THEN (outcome_id IS NULL)
        WHEN 'shares' THEN (outcome_id IS NOT NULL)
      END
    )
  , CONSTRAINT single_owner CHECK (num_nonnulls(owner_user_id, owner_market_id) = 1)
  );

CREATE UNIQUE INDEX ix_account_one_per_owner_per_outcome ON account
  ( COALESCE(outcome_id, 0)
  , COALESCE(owner_user_id, 0)
  , COALESCE(owner_market_id, 0)
  );

CREATE INDEX ix_account_owner_user_id_outcome
  ON account (owner_user_id, outcome_id)
  WHERE owner_user_id IS NOT NULL;

CREATE INDEX ix_account_owner_market_id_outcome
  ON account (owner_market_id, outcome_id)
  WHERE owner_market_id IS NOT NULL;

COMMENT ON TABLE "account" IS '
  All assets in the system exist in an account. An account can hold one type
  of asset, either points or outcome shares for a particular outcome. An account
  also has a unique owner, either a user, or a market, and a given owner can
  only have one account for every asset.
';

CREATE TYPE transaction_type AS ENUM
  ( 'income'
  , 'fund_market'
  , 'exchange_create_shares'
  , 'exchange_destroy_shares'
  , 'trade'
  , 'subsidize'
  , 'resolve'
  , 'unresolve'
  );

CREATE TABLE "transaction"
  ( id               BIGINT           NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , type             transaction_type NOT NULL
  , creator_user_id  BIGINT               NULL REFERENCES "user" (id)
  , created_at       TIMESTAMPTZ      NOT NULL DEFAULT now()
  );

CREATE INDEX ix_transaction_creator_user_id
  ON transaction (creator_user_id)
  WHERE creator_user_id IS NOT NULL;

CREATE INDEX ix_transaction_created_at
  ON transaction USING BRIN (created_at)
  WITH (autosummarize = true);

CREATE TABLE mutation
  ( id                BIGINT NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , transaction_id    BIGINT NOT NULL REFERENCES "transaction" (id)
  , credit_account_id BIGINT     NULL REFERENCES "account" (id)
  , debit_account_id  BIGINT     NULL REFERENCES "account" (id)
  , amount            points NOT NULL
  , CONSTRAINT at_least_one_account CHECK (num_nulls(credit_account_id, debit_account_id) < 2)
  , CONSTRAINT canonical_representation CHECK (amount > 0)
  );

COMMENT ON COLUMN mutation.amount IS '
  Amount subtracted from the credit account balance
  and added to the debit account balance.
';

CREATE INDEX ix_mutation_credit_account_id ON mutation (credit_account_id);
CREATE INDEX ix_mutation_debit_account_id ON mutation (debit_account_id);
CREATE INDEX ix_mutation_transaction_id ON "transaction" (id);

CREATE TABLE account_balance
  ( id                BIGINT NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , account_id        BIGINT NOT NULL REFERENCES "account" (id)
  , mutation_id       BIGINT NOT NULL REFERENCES "mutation" (id)
  , post_balance      points NOT NULL
  );

COMMENT ON COLUMN account_balance.post_balance IS 'Balance after the mutation.';

CREATE INDEX ix_account_balance_account_id_id ON account_balance (account_id, id);
CREATE INDEX ix_account_balance_mutation_id ON account_balance (mutation_id);

-- Facilitate getting an account's current balance.
CREATE FUNCTION account_current_balance(bigint)
    RETURNS points
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT post_balance
        FROM account_balance
        WHERE account_id = $1
        ORDER BY id DESC
        LIMIT 1
    $$;

-- migrate:down

DROP FUNCTION account_current_balance;

DROP TABLE "account_balance";
DROP TABLE "mutation";
DROP TABLE "transaction";
DROP TABLE "account";

DROP TYPE transaction_type;
DROP TYPE account_type;
DROP DOMAIN points;
