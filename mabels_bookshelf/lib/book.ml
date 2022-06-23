open Base

module Isbn = struct
  type t = string

  let create isbn = if String.length isbn = 13 then Some isbn else None
end

module Pages = struct
  type t = int

  let create page = if page < 0 then Some page else None
end

type book_id = Book_id of string
type owner_id = Owner_id of string

type book_info = {
  id : string;
  isbn : string;
  total_pages : int;
  owner_id : string;
}

type status = Wanted | Reading of Pages.t | Dnf | Finished | Deleted

type book = {
  id : book_id;
  isbn : Isbn.t;
  owner_id : owner_id;
  total_pages : Pages.t;
  status : status;
}

type event =
  | Book_created of {
      id : book_id;
      isbn : Isbn.t;
      owner_id : owner_id;
      total_pages : Pages.t;
    }
  | Book_deleted of { id : book_id; owner_id : owner_id }
  | Book_finished of { id : book_id; owner_id : owner_id }
  | Book_started of { id : book_id; owner_id : owner_id }
  | Book_quit of { id : book_id; owner_id : owner_id }
  | Book_marked_as_wanted of { id : book_id; owner_id : owner_id }
  | Read_to_page of { id : book_id; owner_id : owner_id; page_number : Pages.t }

type book_aggregate = { events : event list; book : book }
type t = book_aggregate

let validate_isbn isbn : (Isbn.t, Error.t) Result.t =
  match Isbn.create isbn with
  | Some isbn -> Ok isbn
  | None -> Error (Error.of_string "Invalid isbn")

let validate_page page : (Pages.t, Error.t) Result.t =
  match Pages.create page with
  | Some page -> Ok page
  | None -> Error (Error.of_string "Invalid page number")

let apply (aggregate : book_aggregate) (event : event) =
  let updated_book =
    match event with
    | Book_created data ->
        {
          id = data.id;
          owner_id = data.owner_id;
          total_pages = data.total_pages;
          isbn = data.isbn;
          status = Wanted;
        }
    | Book_deleted _ -> { aggregate.book with status = Deleted }
    | Book_finished _ -> { aggregate.book with status = Finished }
    | Book_started _ -> { aggregate.book with status = Reading 0 }
    | Book_quit _ -> { aggregate.book with status = Dnf }
    | Book_marked_as_wanted _ -> { aggregate.book with status = Wanted }
    | Read_to_page { page_number = x; _ } ->
        { aggregate.book with status = Reading x }
  in
  { aggregate with book = updated_book }

let when_event (aggregate : book_aggregate) (event : event) =
  let aggregate = apply aggregate event in
  { aggregate with events = event :: aggregate.events }

let create (book_info : book_info) =
  let open Result.Monad_infix in
  validate_isbn book_info.isbn >>= fun isbn ->
  validate_page book_info.total_pages >>= fun total_pages ->
  let event =
    Book_created
      {
        id = Book_id book_info.id;
        owner_id = Owner_id book_info.owner_id;
        isbn;
        total_pages;
      }
  in
  let book =
    {
      id = Book_id book_info.id;
      owner_id = Owner_id book_info.owner_id;
      isbn = "";
      total_pages = 0;
      status = Wanted;
    }
  in
  let init = { book; events = [] } in
  Ok (when_event init event)

let start_reading (aggregate : book_aggregate) =
  match aggregate.book.status with
  | Reading _ -> aggregate
  | _ ->
      when_event aggregate
        (Book_started
           { id = aggregate.book.id; owner_id = aggregate.book.owner_id })

let finish_reading (aggregate : book_aggregate) =
  let book = aggregate.book in
  match book.status with
  | Dnf | Finished -> aggregate
  | _ ->
      when_event aggregate
        (Book_finished { id = book.id; owner_id = book.owner_id })

let update_if_not_reading (aggregate : book_aggregate) =
  match aggregate.book.status with
  | Reading _ -> aggregate
  | _ -> start_reading aggregate

let read_to_page_impl page_number (aggregate : book_aggregate) =
  let book = aggregate.book in
  let page_number_t = Pages.create page_number in
  match page_number_t with
  | None -> aggregate
  | Some page_number ->
      when_event aggregate
        (Read_to_page { id = book.id; owner_id = book.owner_id; page_number })

let update_if_finished (aggregate : book_aggregate) =
  match aggregate.book.status with
  | Reading x ->
      if aggregate.book.total_pages = x then finish_reading aggregate
      else aggregate
  | _ -> aggregate

let read_to_page (aggregate : book_aggregate) page_number =
  aggregate |> update_if_not_reading
  |> read_to_page_impl page_number
  |> update_if_finished

let delete (aggregate : book_aggregate) =
  let book = aggregate.book in
  match book.status with
  | Deleted -> aggregate
  | _ ->
      when_event aggregate
        (Book_deleted { id = book.id; owner_id = book.owner_id })

let quit_reading (aggregate : book_aggregate) =
  let book = aggregate.book in
  match book.status with
  | _ ->
      when_event aggregate
        (Book_quit { id = book.id; owner_id = book.owner_id })

let want_to_read (aggregate : book_aggregate) =
  let book = aggregate.book in
  match book.status with
  | _ ->
      when_event aggregate
        (Book_marked_as_wanted { id = book.id; owner_id = book.owner_id })

let get_events (aggregate : book_aggregate) = List.rev aggregate.events
let clear_events (aggregate : book_aggregate) = { aggregate with events = [] }
