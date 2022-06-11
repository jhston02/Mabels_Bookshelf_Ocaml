open Base
open Core

module Isbn = struct 
  type t = int
end

module Pages = struct
  type t = int
end

type book_id = BookId of string

type owner_id = OwnerId of string

type core_book_data = 
{
  id : book_id;
  isbn : Isbn.t;
  owner_id : owner_id;
  total_page : Pages.t;
}

type book = 
  | WantedBook of core_book_data
  | ReadingBook of (core_book_data * Pages.t)
  | QuitBook of core_book_data
  | FinishedBook of core_book_data

type t = book

let create book_info = 
