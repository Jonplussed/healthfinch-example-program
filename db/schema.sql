-- required extensions

CREATE EXTENSION citext;



-- shared stored procedures

CREATE FUNCTION on_record_insert() RETURNS trigger AS $$
  DECLARE
    id_sequence VARCHAR;
  BEGIN
    SELECT TG_ARGV[0] INTO id_sequence;     -- the name of the ID sequence for this table
    NEW.id         := nextval(id_sequence); -- set the ID as the next sequence value
    NEW.created_at := now();
    NEW.updated_at := now();
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION on_record_update() RETURNS trigger AS $$
  BEGIN
    NEW.id         := OLD.id;         -- ensure IDs aren't altered on updates
    NEW.created_at := OLD.created_at; -- ensure date-created isn't altered on updates
    NEW.updated_at := now();
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;



-- the URLs resource

CREATE TABLE urls (
  id           INTEGER    PRIMARY KEY,
  url          CITEXT     NOT NULL,
  created_at   TIMESTAMP  NOT NULL,
  updated_at   TIMESTAMP  NOT NULL
);

CREATE UNIQUE INDEX url_on_urls
  ON urls (url);

CREATE SEQUENCE urls_ids START 1;

CREATE TRIGGER urls_insert
  BEFORE INSERT ON urls
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('urls_ids');

CREATE TRIGGER urls_update
  BEFORE UPDATE ON urls
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();



-- the words resource

CREATE TABLE words (
  id           INTEGER    PRIMARY KEY,
  url_id       INTEGER    REFERENCES urls ON DELETE RESTRICT,
  word         CITEXT     NOT NULL,
  count        INTEGER    NOT NULL,
  created_at   TIMESTAMP  NOT NULL,
  updated_at   TIMESTAMP  NOT NULL
);

CREATE UNIQUE INDEX url_id_and_word_on_words
  ON words (url_id, word);

CREATE SEQUENCE words_ids START 1;

CREATE TRIGGER words_insert
  BEFORE INSERT ON words
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('words_ids');

CREATE TRIGGER words_update
  BEFORE UPDATE ON words
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();
