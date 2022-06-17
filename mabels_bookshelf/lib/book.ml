open Base

module Isbn = struct 
  type t = string

  let create isbn = 
    if (String.length isbn) = 13 then
      Some(isbn)
    else
      None
end

module Pages = struct
  type t = int

  let create page =
    if page < 0 then
      Some(page)
    else
      None
end

type book_id = Book_id of string

type owner_id = Owner_id of string

type book_info = 
{
    id : string;
    isbn : string;
    total_pages : int;
    owner_id : string;
}

type core_book_data = 
{
  id : book_id;
  isbn : Isbn.t;
  owner_id : owner_id;
  total_page : Pages.t;
}

type event =
  | Book_created of core_book_data
  | Book_deleted of { id : book_id; owner_id : owner_id}
  | Book_finished of { id : book_id; owner_id : owner_id}
  | Book_started of { id : book_id; owner_id : owner_id}
  | Book_quit of { id : book_id; owner_id : owner_id}
  | Book_marked_as_wanted of { id: book_id; owner_id : owner_id}
  | Read_to_page of { id : book_id; owner_id : owner_id; page_number : Pages.t}

type book = 
  | Empty
  | Wanted_book of (core_book_data * event list)
  | Reading_book of (core_book_data * Pages.t * event list)
  | Quit_book of core_book_data * event list
  | Finished_book of core_book_data * event list


type t = book


let validate_isbn isbn : (Isbn.t, Error.t) Result.t =
  match Isbn.create isbn with
  | Some isbn -> Ok(isbn)
  | None -> Error(Error.of_string "Invalid isbn")

let validate_page page : (Pages.t, Error.t) Result.t = 
  match Pages.create page with
  | Some page -> Ok(page)
  | None -> Error(Error.of_string "Invalid page number")

let empty = Empty 

let apply (book:book) (event:event) = 
  match event with
  | Book_created data  -> Wanted_book (data, [event])
  | _ -> Empty

let create (book_info:book_info) =
  let open Result.Monad_infix in
  validate_isbn book_info.isbn >>= fun isbn ->
  validate_page book_info.total_pages >>= fun total_pages ->
  let event = Book_created { id = Book_id(book_info.id); owner_id = Owner_id(book_info.owner_id); isbn = isbn; total_page = total_pages} in 
  let init = Empty in
  Ok (apply init event)


