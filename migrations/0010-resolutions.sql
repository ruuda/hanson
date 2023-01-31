-- migrate:up

CREATE TABLE resolutions
  ( id         BIGINT      NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , market_id  BIGINT      NOT NULL REFERENCES markets (id)
  , resolver   BIGINT      NOT NULL REFERENCES users (id)
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  );

CREATE INDEX ix_resolutions_market_id_id ON resolutions (market_id, id);

CREATE TABLE resolution_values
  ( id            BIGINT   NOT NULL PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY
  , resolution_id BIGINT   NOT NULL REFERENCES resolutions (id)
  , outcome_id    BIGINT   NOT NULL REFERENCES outcomes (id)
  , "value"       points   NOT NULL
  );

CREATE INDEX ix_resolution_values_resolution_id ON resolution_values (resolution_id);

DROP VIEW markets_ext;
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
  ) as current_description,
  (
    SELECT id
    FROM resolutions
    WHERE market_id = markets.id
    ORDER BY id DESC
    LIMIT 1
  ) as current_resolution_id
FROM
  markets;

-- migrate:down

-- Last defined in migration 0009.
DROP VIEW markets_ext;
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

DROP TABLE resolution_values;
DROP TABLE resolutions;