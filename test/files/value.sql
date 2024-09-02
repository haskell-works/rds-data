INSERT INTO all_types (
  the_bigint,
  the_bigserial,
  the_bit,
  the_bits,
  the_varying_bit,
  the_varying_bits,
  the_boolean,
  the_box,
  the_bytea,
  the_character,
  the_characters,
  the_varying_character,
  the_varying_characters,
  the_cidr,
  the_circle,
  the_date,
  the_double,
  the_inet,
  the_integer,
  the_json,
  the_jsonb,
  the_line,
  the_lseg,
  the_macaddr,
  the_macaddr8,
  the_money,
  the_numeric,
  the_numerics,
  the_path,
  the_point,
  the_polygon,
  the_real,
  the_smallint,
  the_smallserial,
  the_serial,
  the_text,
  the_time,
  the_times,
  the_timestamp,
  the_timestamps,
  the_tsquery,
  the_tsvector,
  the_uuid,
  the_xml
) VALUES (
  1234567890,                             -- the_bigint
  DEFAULT,                                -- the_bigserial
  B'1',                                   -- the_bit
  B'10',                                  -- the_bits
  B'0',                                   -- the_varying_bit
  B'11',                                  -- the_varying_bits
  TRUE,                                   -- the_boolean
  '((1,1),(2,2))',                        -- the_box
  E'\\x1234',                             -- the_bytea
  'A',                                    -- the_character
  'AB',                                   -- the_characters
  'C',                                    -- the_varying_character
  'CD',                                   -- the_varying_characters
  '192.168.1.0/24',                       -- the_cidr
  '((0,0),2)',                            -- the_circle
  '2024-02-04',                           -- the_date
  3.14159265359,                          -- the_double
  '192.168.0.1',                          -- the_inet
  42,                                     -- the_integer
  '{"key":"value"}',                      -- the_json
  '{"key":"value"}',                      -- the_jsonb
  '((1,1),(2,2))',                        -- the_line
  '[(1,2),(3,4)]',                        -- the_lseg
  '00:11:22:33:44:55',                    -- the_macaddr
  '00:11:22:33:44:55:66:77',              -- the_macaddr8
  12345.67,                               -- the_money
  1234.56,                                -- the_numeric
  12.34,                                  -- the_numerics
  '[(1,2),(3,4)]',                        -- the_path
  '(5,6)',                                -- the_point
  '((1,2),(3,4))',                        -- the_polygon
  3.14,                                   -- the_real
  12345,                                  -- the_smallint
  DEFAULT,                                -- the_smallserial
  DEFAULT,                                -- the_serial
  'Some text',                            -- the_text
  '12:34:56',                             -- the_time
  '12:34:56.78',                          -- the_times
  '2024-02-04 12:34:56',                  -- the_timestamp
  '2024-02-04 12:34:56',                  -- the_timestamps
  to_tsquery('english', 'query'),         -- the_tsquery
  to_tsvector('english', 'document'),     -- the_tsvector
  '0123456789ABCDEFGHJKMNPQRS',           -- the_ulid
  '550e8400-e29b-41d4-a716-446655440000', -- the_uuid
  '<root><element>data</element></root>'  -- the_xml
);
