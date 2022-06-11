open Base

type t

type event = 
    | Book_created
    | Book_deleted
    | Book_finished
    | Book_started
    | Book_quit
    | Book_marked_as_wanted
    | Read_to_page

type validation_error =
    | Invalid_page_number of string
    | Invalid_book_information of string

type book_info = 
{
    id : string;
    isbn : string;
    total_pages : int;
    owner_id : string;
}

val create : book_info -> (t, validation_error) Result.t 

val start_reading : t -> t

val finish_reading : t -> t

val quit_reading : t -> t

val read_to_page : t -> int -> t

val want_to_read : t -> t

val delete : t -> t
