CREATE TABLE genre
(
  id   INT          NOT NULL,
  name VARCHAR(255) NULL    ,
  PRIMARY KEY (id)
);

CREATE TABLE movie
(
  id      INT          NOT NULL,
  title   VARCHAR(255) NULL    ,
  year    INT          NULL    ,
  runtime INT          NULL    ,
  rating  FLOAT        NULL    ,
  PRIMARY KEY (id)
);

CREATE TABLE movie_genre
(
  movie_id INT NOT NULL,
  genre_id INT NOT NULL,
  PRIMARY KEY (movie_id, genre_id)
);

ALTER TABLE movie_genre
  ADD CONSTRAINT FK_movie_TO_movie_genre
    FOREIGN KEY (movie_id)
    REFERENCES movie (id);

ALTER TABLE movie_genre
  ADD CONSTRAINT FK_genre_TO_movie_genre
    FOREIGN KEY (genre_id)
    REFERENCES genre (id);