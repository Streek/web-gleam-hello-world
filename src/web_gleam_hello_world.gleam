import gleam/bytes_builder.{type BytesBuilder}
import gleam/erlang/process
import gleam/http/cowboy
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}

// Define a HTTP service
//
pub fn my_service(request: Request(t)) -> Response(BytesBuilder) {
  let body = bytes_builder.from_string("Hello, world!")

  response.new(200)
  |> response.prepend_header("made-with", "Gleam")
  |> response.set_body(body)
}

// Start it on port 3000!
//
pub fn main() {
  cowboy.start(my_service, on_port: 8888)
  process.sleep_forever()
}
