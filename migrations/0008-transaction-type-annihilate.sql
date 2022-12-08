-- migrate:up

ALTER TYPE transaction_type ADD VALUE 'annihilate';

-- migrate:down

ALTER TYPE transaction_type RENAME VALUE 'annihilate' TO '_deprecated_annihilate';
