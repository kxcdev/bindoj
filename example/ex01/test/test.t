Start the server program in the background.
  $ dune exe ../src/server/server.exe > /dev/null &
  $ PORT=8081

Capture it's process id.
  $ PID=`echo $!`

Wait (max 5 sec) for server to start
  $ TIMEOUT=5
  $ URL=localhost:$PORT/_health
  $ ./wait_server_up.sh "$URL"
  # waiting (up to 5 sec) for localhost:8081/_health
  "i am not a tea pot"

Start the client program.
  $ dune exe ../src/client/client.exe
  get_any_student:
  { admission_year = 1984; name = "William Gibson" }
  get_student_from_person:
  { admission_year = 451; name = "Ray Bradbury" }

Check endpoints
  $ curl -sf localhost:$PORT/student/any-one
  {"admissionYear":1984,"name":"William Gibson"}

Check endpoints
  $ curl -sf localhost:$PORT/student/from-person \
  >   -d '{"kind":"student","studentId":451,"name":"Ray Bradbury"}'
  {"admissionYear":451,"name":"Ray Bradbury"}

Check endpoints
  $ curl -si localhost:$PORT/student/from-person \
  >   -d '{"kind":"with-id","value":0}' \
  >   | grep -v 'connection: '
  HTTP/1.1 422 Status 422
  Content-Type: application/json
  content-length: 22
  
  "with_id, not student"

Check endpoints
  $ curl -si localhost:$PORT/student/from-person \
  >   -d '{"kind":"student","studentId":0}' \
  >   | grep -v 'connection: '
  HTTP/1.1 400 Status 400
  Content-Type: application/json
  content-length: 1611
  
  "Bad request: invalid json format - mandatory field 'name' does not exist at root; expected shape: `with_warning (\n(\"not considering any config if exists\",\n `named ((\"Person\",\n          `anyone_of ([`object_of ([`mandatory_field ((\"kind\",\n                                                       `exactly (\"anonymous\")))\n                                     ]);\n                        `object_of ([`mandatory_field ((\"kind\",\n                                                        `exactly (\"with-id\")));\n                                      `mandatory_field ((\"value\", `integral))\n                                      ]);\n                        `object_of ([`mandatory_field ((\"kind\",\n                                                        `exactly (\"student\")));\n                                      `mandatory_field ((\"studentId\",\n                                                         `integral));\n                                      `mandatory_field ((\"name\", `string))]);\n                        `object_of ([`mandatory_field ((\"kind\",\n                                                        `exactly (\"teacher\")));\n                                      `mandatory_field ((\"facultyId\",\n                                                         `integral));\n                                      `mandatory_field ((\"name\", `string));\n                                      `mandatory_field ((\"department\",\n                                                         `string))\n                                      ])\n                        ])))))"

Kill the server process in the background.
  $ kill -9 $PID
