open Base

type t

type event

type book_info = 
{
    id : string;
    isbn : string;
    total_pages : int;
    owner_id : string;
}

val empty : t

val create : book_info -> (t, Error.t) Result.t 

val start_reading : t -> t

val finish_reading : t -> t

val quit_reading : t -> t

val read_to_page : t -> int -> t

val want_to_read : t -> t

val delete : t -> t

val apply : t -> event -> t

val get_events : t -> event list

val clear_events : t -> t
