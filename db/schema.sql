-- database schema

CREATE EXTENSION citext;

CREATE TABLE histogram (
  url          CITEXT     NOT NULL,
  word         CITEXT     NOT NULL,
  count        INTEGER    NOT NULL
);

CREATE UNIQUE INDEX url_and_word_on_histogram
  ON histogram (url, word);
