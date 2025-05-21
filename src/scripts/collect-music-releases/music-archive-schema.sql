CREATE TABLE album_artist
(
  album_id INT NOT NULL,
  artist_id  INT NOT NULL,
  PRIMARY KEY (album_id, artist_id)
);

CREATE TABLE album_genre
(
  album_id INT NOT NULL,
  genre_id   INT NOT NULL,
  PRIMARY KEY (album_id, genre_id)
);

CREATE TABLE album_style
(
  album_id INT NOT NULL,
  style_id   INT NOT NULL,
  PRIMARY KEY (album_id, style_id)
);

CREATE TABLE artist
(
  id   INT     NOT NULL,
  name VARCHAR(255) NULL,
  born INT     NULL,
  died INT     NULL,
  PRIMARY KEY (id)
);

CREATE TABLE genre
(
  id    INT     NOT NULL,
  genre VARCHAR(255) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE artist_part_of
(
  member_id INT NOT NULL,
  band_id   INT NOT NULL,
  PRIMARY KEY (member_id, band_id)
);

CREATE TABLE album
(
  id      INT     NOT NULL,
  artist_id INT NOT NULL,
  title   VARCHAR(255) NULL,
  year    INT     NULL,
  country VARCHAR(255) NULL,
  PRIMARY KEY (id)
);

CREATE TABLE style
(
  id    INT     NOT NULL,
  style VARCHAR(255) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE track
(
  album_id INT     NOT NULL,
  number    INT     NOT NULL,
  position   VARCHAR(255) NULL,
  title      VARCHAR(255) NULL,
  duration   INT     NULL,
  PRIMARY KEY (album_id, number)
);

ALTER TABLE album
  ADD CONSTRAINT FK_artist_id_TO_artist_description
    FOREIGN KEY (artist_id)
    REFERENCES artist (id);

ALTER TABLE album_description
  ADD CONSTRAINT FK_album_TO_album_description
    FOREIGN KEY (album_id)
    REFERENCES album (id);

ALTER TABLE album_genre
  ADD CONSTRAINT FK_album_TO_album_genre
    FOREIGN KEY (album_id)
    REFERENCES album (id);

ALTER TABLE album_genre
  ADD CONSTRAINT FK_genre_TO_album_genre
    FOREIGN KEY (genre_id)
    REFERENCES genre (id);

ALTER TABLE album_style
  ADD CONSTRAINT FK_album_TO_album_style
    FOREIGN KEY (album_id)
    REFERENCES album (id);

ALTER TABLE album_style
  ADD CONSTRAINT FK_style_TO_album_style
    FOREIGN KEY (style_id)
    REFERENCES style (id);

ALTER TABLE artist_part_of
  ADD CONSTRAINT FK_artist_TO_part_of
    FOREIGN KEY (member_id)
    REFERENCES artist (id);

ALTER TABLE artist_part_of
  ADD CONSTRAINT FK_artist_TO_part_of1
    FOREIGN KEY (band_id)
    REFERENCES artist (id);

ALTER TABLE track
  ADD CONSTRAINT FK_album_TO_track
    FOREIGN KEY (album_id)
    REFERENCES album (id);
