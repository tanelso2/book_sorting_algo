open Core

type book = {
    author: string list;
    title: string;
}

type book_score = (book * float)

let compare_books_scores (a: book_score) (b: book_score): int =
    let (_, a_score) = a in
    let (_, b_score) = b in
    Float.compare a_score b_score

type connection =
    | AuthorConnection of string * string
    | FunctionConnection of (book -> book -> float)

(* TODO: Play around with different scores *)
let author_connection_default_score = 50.0

let shared_authors_count {author=author_x; _} {author=author_y; _}: int =
  let shared_author_list = List.filter_map author_x ~f:(fun x -> if List.mem author_y x ~equal:String.equal then Some x else None) in
  List.length shared_author_list


let score_connection (a: book) (b: book) (c: connection): float =
    match c with
    | FunctionConnection (f) -> f a b
    | AuthorConnection (author_1, author_2) ->
      let contains = List.mem ~equal:String.equal in
      let { author = author_a; _ } = a in
      let { author = author_b; _ } = b in
      if (contains author_a author_1 && contains author_b author_2) || (contains author_a author_2 && contains author_b author_1)
      then author_connection_default_score
      else 0.0

let book_connection_score (a: book) (b: book) (connections: connection list): float =
    List.fold connections ~init:0.0 ~f:(fun accum c -> accum +. (score_connection a b c))

(* TODO: Play around with different decreasing functions *)
let (decrease_score:(float -> float)) = fun x -> x /. 2.0

let update_score (curr: book_score) (connections: connection list) (b: book): book_score =
    let (curr_book, curr_score) = curr in
    (* Decrease the previous score so that the most recent book has the
     * most effect on the score *)
    let reduced_score = decrease_score curr_score in
    let new_score = reduced_score +. (book_connection_score curr_book b connections) in
    (curr_book, new_score)

let update_scores (l: book_score list) (connections: connection list) (b: book): book_score list =
    List.map l ~f:(fun x -> update_score x connections b)

let rec find_ordering_helper (current: book list) (connections: connection list) (unused_books: book_score list): book list =
  match List.sort unused_books ~compare:(fun a b -> Int.neg @@ compare_books_scores a b) with
  | [] -> current
  | (next_book, _)::rest ->
    let new_unused = update_scores rest connections next_book in
    let new_list = next_book::current in
    find_ordering_helper new_list connections new_unused


let () = Random.self_init ()

let find_ordering (books: book list) (connections: connection list): book list =
  (* TODO: Scan books and add connections for co-authors *)
    let starting_book = List.random_element_exn books in
    let rest = List.filter books ~f:(fun x -> not (phys_equal x starting_book)) in
    let rest_scores = List.map rest ~f:(fun x -> (x, 0.0)) in
    let first_scores = update_scores rest_scores connections starting_book in
    find_ordering_helper [starting_book] connections first_scores


let test_connections = [
  FunctionConnection (fun x y -> 100.0 *. (Float.of_int (shared_authors_count x y)));
  AuthorConnection ("George Orwell", "Aldous Huxley") ;
  AuthorConnection ("Haruki Murakami", "Kurt Vonnegut") ;
  AuthorConnection ("Tolkien", "JK Rowling") ;
  AuthorConnection ("JK Rowling", "Susanne Clark") ;
  AuthorConnection ("Susanne Clark", "George RR Martin") ;
]

let test_books = [
  { author = ["George Orwell"]; title = "1984"; };
  { author = ["George Orwell"]; title = "Animal Farm"; };
  { author = ["Aldous Huxley"]; title = "Brave New World"; };
  { author = ["Aldous Huxley"]; title = "Island"; };
  { author = ["Haruki Murakami"]; title = "Norwegian Wood"; };
  { author = ["Haruki Murakami"]; title = "1Q84"; };
  { author = ["Haruki Murakami"]; title = "Kafka on the Shore"; };
  { author = ["Kurt Vonnegut"]; title = "Slaughterhouse 5"; };
  { author = ["Kurt Vonnegut"]; title = "Cat's Cradle"; };
  { author = ["Kurt Vonnegut"]; title = "Mother Night"; };
  { author = ["Kurt Vonnegut"]; title = "Bluebeard"; };
  { author = ["Kurt Vonnegut"]; title = "Player Piano"; };
  { author = ["Kurt Vonnegut"]; title = "Galapagos"; };
  { author = ["JK Rowling"]; title = "Harry Potter"; };
  { author = ["JRR Tolkien"]; title = "The Hobbit"; };
  { author = ["JRR Tolkien"]; title = "Lord of the Rings"; };
  { author = ["Susanne Clark"]; title = "Jonathon Strange & Mr. Norrell"; };
  { author = ["George RR Martin"]; title = "A Song of Ice and Fire"; };
]

(* TODO: Put all this parsing logic in a different module *)
(* TODO: Ok yaml might have been a mistake here. How about just two files, "books.csv" and "connections.csv" and each line is a book or connection *)
(* TODO: Shorthand (string, string list) Map.t for author -> title bindings? *)
let input_file = "input.yaml"

let yaml_to_string_exn (s: Yaml.value): string =
  match s with
  | `String x -> x
  | _ -> failwith "Non-string yaml object passed to `yaml_to_string_exn`"

let yaml_map_exn (y: Yaml.value) ~(f:(Yaml.value -> 'a)): 'a list =
  match y with
  | `A l -> List.map l ~f:f
  | _ -> failwith "yaml_map_exn was passed a non-array"

let convert_connection_yaml = function
  | `A (a::b::[]) -> AuthorConnection (yaml_to_string_exn a, yaml_to_string_exn b)
  | _ -> failwith "Every connection should be defined as an array of 2 elements"

let convert_book_yaml (y: Yaml.value): book =
  let failure_message = "Every book should be defined as an object with a string title field and a list of strings author field" in
  match y with
  | `O assoc_list ->
    let dict = String.Map.of_alist_exn assoc_list in
    (match (String.Map.find dict "title", String.Map.find dict "author") with
    | (Some title_yaml, Some authors_yaml) ->
        {
          author = yaml_map_exn authors_yaml ~f:yaml_to_string_exn;
          title = yaml_to_string_exn title_yaml
        }
    | _ -> failwith failure_message)
  | _ -> failwith failure_message

let parse_input (): (connection list * book list) =
  match Yaml_unix.of_file_exn @@ Fpath.v input_file with
  | `O (assoc_list) ->
    let dict = String.Map.of_alist_exn assoc_list in
    let connections_yaml = String.Map.find_exn dict "connections" in
    let books_yaml = String.Map.find_exn dict "books" in
    (yaml_map_exn connections_yaml ~f:convert_connection_yaml, yaml_map_exn books_yaml ~f:convert_book_yaml)
  | _ -> failwith "Expected an object at top level"


let print_book {author; title}: unit =
  let authors = String.concat ~sep:", " author in
  Printf.printf "%s by %s\n" title authors

let default_connections = [
  FunctionConnection (fun x y -> 100.0 *. (Float.of_int (shared_authors_count x y)));
]

let () =
  let connections, books = parse_input () in
  let all_connections = connections @ default_connections in
  List.iter ~f:print_book @@ find_ordering books all_connections
