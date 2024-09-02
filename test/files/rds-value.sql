INSERT INTO all_types (
  the_bigint,
  the_bigserial,
  the_boolean,
  the_bytea,
  the_character,
  the_characters,
  the_varying_character,
  the_varying_characters,
  the_date,
  the_double,
  the_integer,
  the_json,
  the_jsonb,
  the_numeric,
  the_numerics,
  the_real,
  the_smallint,
  the_smallserial,
  the_serial,
  the_text,
  the_time,
  the_times,
  the_timestamp,
  the_timestamps,
  the_uuid
) VALUES (
  1234567890,                             -- the_bigint
  DEFAULT,                                -- the_bigserial
  TRUE,                                   -- the_boolean
  E'\\x1234',                             -- the_bytea
  'A',                                    -- the_character
  'AB',                                   -- the_characters
  'C',                                    -- the_varying_character
  'CD',                                   -- the_varying_characters
  '2024-02-04',                           -- the_date
  3.14159265359,                          -- the_double
  42,                                     -- the_integer
  '{"key":"value"}',                      -- the_json
  '{"key":"value"}',                      -- the_jsonb
  1234.56,                                -- the_numeric
  12.34,                                  -- the_numerics
  3.14,                                   -- the_real
  12345,                                  -- the_smallint
  DEFAULT,                                -- the_smallserial
  DEFAULT,                                -- the_serial
  'Some text',                            -- the_text
  '12:34:56',                             -- the_time
  '12:34:56.78',                          -- the_times
  '2024-02-04 12:34:56',                  -- the_timestamp
  '2024-02-04 12:34:56',                  -- the_timestamps
  '0123456789ABCDEFGHJKMNPQRS',           -- the_ulid
  '550e8400-e29b-41d4-a716-446655440000'  -- the_uuid
);
