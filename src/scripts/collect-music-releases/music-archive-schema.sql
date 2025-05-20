
CREATE TABLE album_artist
(
  release_id INT NOT NULL,
  artist_id  INT NOT NULL,
  PRIMARY KEY (release_id, artist_id)
);

CREATE TABLE album_description
(
  release_id     INT NOT NULL,
  description_id INT NOT NULL,
  PRIMARY KEY (release_id, description_id)
);

CREATE TABLE album_genre
(
  release_id INT NOT NULL,
  genre_id   INT NOT NULL,
  PRIMARY KEY (release_id, genre_id)
);

CREATE TABLE album_style
(
  release_id INT NOT NULL,
  style_id   INT NOT NULL,
  PRIMARY KEY (release_id, style_id)
);

CREATE TABLE artist
(
  id   INT     NOT NULL,
  name VARCHAR NULL    ,
  born INT     NULL    ,
  died INT     NULL    ,
  PRIMARY KEY (id)
);

CREATE TABLE description
(
  id          INT     NOT NULL,
  description VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE genre
(
  id    INT     NOT NULL,
  genre VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE part_of
(
  member_id INT NOT NULL,
  band_id   INT NOT NULL,
  PRIMARY KEY (member_id, band_id)
);

CREATE TABLE release
(
  id      INT     NOT NULL,
  title   VARCHAR NULL    ,
  year    INT     NULL    ,
  country VARCHAR NULL    ,
  PRIMARY KEY (id)
);

CREATE TABLE style
(
  id    INT     NOT NULL,
  style VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE track
(
  release_id INT     NOT NULL,
  index      INT     NOT NULL,
  position   VARCHAR NULL    ,
  title      VARCHAR NULL    ,
  duration   INT     NULL    ,
  PRIMARY KEY (release_id, index)
);

ALTER TABLE album_description
  ADD CONSTRAINT FK_release_TO_album_description
    FOREIGN KEY (release_id)
    REFERENCES release (id);

ALTER TABLE album_description
  ADD CONSTRAINT FK_description_TO_album_description
    FOREIGN KEY (description_id)
    REFERENCES description (id);

ALTER TABLE album_genre
  ADD CONSTRAINT FK_release_TO_album_genre
    FOREIGN KEY (release_id)
    REFERENCES release (id);

ALTER TABLE album_genre
  ADD CONSTRAINT FK_genre_TO_album_genre
    FOREIGN KEY (genre_id)
    REFERENCES genre (id);

ALTER TABLE album_style
  ADD CONSTRAINT FK_release_TO_album_style
    FOREIGN KEY (release_id)
    REFERENCES release (id);

ALTER TABLE album_style
  ADD CONSTRAINT FK_style_TO_album_style
    FOREIGN KEY (style_id)
    REFERENCES style (id);

ALTER TABLE album_artist
  ADD CONSTRAINT FK_release_TO_album_artist
    FOREIGN KEY (release_id)
    REFERENCES release (id);

ALTER TABLE album_artist
  ADD CONSTRAINT FK_artist_TO_album_artist
    FOREIGN KEY (artist_id)
    REFERENCES artist (id);

ALTER TABLE part_of
  ADD CONSTRAINT FK_artist_TO_part_of
    FOREIGN KEY (member_id)
    REFERENCES artist (id);

ALTER TABLE part_of
  ADD CONSTRAINT FK_artist_TO_part_of1
    FOREIGN KEY (band_id)
    REFERENCES artist (id);

ALTER TABLE track
  ADD CONSTRAINT FK_release_TO_track
    FOREIGN KEY (release_id)
    REFERENCES release (id);
