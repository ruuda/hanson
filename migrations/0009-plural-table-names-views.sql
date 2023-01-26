-- migrate:up

DROP FUNCTION account_current_balance;
DROP FUNCTION user_current_username;
DROP FUNCTION user_current_full_name;
DROP FUNCTION market_current_description;
DROP FUNCTION market_current_title;

ALTER TABLE account RENAME TO accounts;
ALTER TABLE account_balance RENAME TO account_balances;
ALTER TABLE market RENAME TO markets;
ALTER TABLE market_description RENAME TO market_descriptions;
ALTER TABLE market_title RENAME TO market_titles;
ALTER TABLE mutation RENAME TO mutations;
ALTER TABLE outcome RENAME TO outcomes;
ALTER TABLE "session" RENAME TO sessions;
ALTER TABLE subtransaction RENAME TO subtransactions;
ALTER TABLE "transaction" RENAME TO transactions;
ALTER TABLE "user" RENAME TO users;
ALTER TABLE user_full_name RENAME TO user_full_names;
ALTER TABLE user_username RENAME TO user_usernames;

CREATE VIEW accounts_ext AS
SELECT
  *,
  COALESCE(
    (
      SELECT post_balance
      FROM account_balances
      WHERE account_id = accounts.id
      ORDER BY id DESC
      LIMIT 1
    ),
    0.00
  ) as current_balance
FROM
  accounts;

CREATE VIEW markets_ext AS
SELECT
  *,
  (
    SELECT title
    FROM market_titles
    WHERE market_id = markets.id
    ORDER BY id DESC
    LIMIT 1
  ) as current_title,
  (
    SELECT description
    FROM market_descriptions
    WHERE market_id = markets.id
    ORDER BY id DESC
    LIMIT 1
  ) as current_description
FROM
  markets;

CREATE VIEW users_ext AS
SELECT
  *,
  (
    SELECT full_name
    FROM user_full_names
    WHERE user_id = users.id
    ORDER BY id DESC
    LIMIT 1
  ) as current_full_name,
  (
    SELECT username
    FROM user_usernames
    WHERE user_id = users.id
    ORDER BY id DESC
    LIMIT 1
  ) as current_username
FROM
  users;

-- migrate:down

DROP VIEW users_ext;
DROP VIEW markets_ext;
DROP VIEW accounts_ext;

ALTER TABLE user_usernames RENAME TO user_username;
ALTER TABLE user_full_names RENAME TO user_full_name;
ALTER TABLE users RENAME TO "user";
ALTER TABLE transactions RENAME TO "transaction";
ALTER TABLE subtransactions RENAME TO subtransaction;
ALTER TABLE sessions RENAME TO "session";
ALTER TABLE outcomes RENAME TO outcome;
ALTER TABLE mutations RENAME TO mutation;
ALTER TABLE market_titles RENAME TO market_title;
ALTER TABLE market_descriptions RENAME TO market_description;
ALTER TABLE markets RENAME TO market;
ALTER TABLE account_balances RENAME TO account_balance;
ALTER TABLE accounts RENAME TO account;

-- See migration 0006 originally.
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

-- See migration 0004 originally.
CREATE FUNCTION user_current_full_name(bigint)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT full_name
        FROM user_full_name
        WHERE user_id = $1
        ORDER BY id DESC
        LIMIT 1
    $$;

-- See migration 0004 originally.
CREATE FUNCTION user_current_username(bigint)
    RETURNS text
    LANGUAGE SQL
    STABLE
    RETURNS NULL ON NULL INPUT
    PARALLEL SAFE
    AS $$
        SELECT username
        FROM user_username
        WHERE user_id = $1
        ORDER BY id DESC
        LIMIT 1
    $$;

-- See migration 0003 originally.
CREATE FUNCTION market_current_title(bigint)
  RETURNS text
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT
  PARALLEL SAFE
  AS $$
    SELECT title
    FROM market_title
    WHERE market_id = $1
    ORDER BY id DESC
    LIMIT 1
  $$;

-- See migration 0003 originally.
CREATE FUNCTION market_current_description(bigint)
  RETURNS text
  LANGUAGE SQL
  STABLE
  RETURNS NULL ON NULL INPUT
  PARALLEL SAFE
  AS $$
    SELECT description
    FROM market_description
    WHERE market_id = $1
    ORDER BY id DESC
    LIMIT 1
  $$;
