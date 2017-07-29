package chapter6

import chapter6.CollectionExercices._

object ForEx {

  def nolanFilms: Seq[String] = for {
    film <- nolan.films
  } yield film.name

  def allFilmNames: Seq[String] = for {
    director <- directors
    film <- director.films
  } yield film.name

  def allFilms: Seq[Film] = for {
    director <- directors
    film <- director.films
  } yield film

  def sortedFilmsByRating: Seq[Film] = allFilms.sortWith((a, b) => a.imdbRating > b.imdbRating)

  def print(): Unit = for {
    director <- directors
    film <- director.films
  } println(s"Tonight only! ${film.name} by ${director.lastName}!")

}

