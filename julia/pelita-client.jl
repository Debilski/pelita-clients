
if length(Base.ARGS) != 1
  error("usage: 'julia ./pelita-client.jl <address>'")
else
  socket_address = Base.ARGS[1]
end

using ZMQ
using JSON

ctx = ZMQ.Context(1)
socket = ZMQ.Socket(ctx, ZMQ.PAIR)
ZMQ.connect(socket, socket_address)


function handle_action(action, data)
  print (action)
  if action == "team_name"
    "Julia Stopping Players"
  elseif action == "set_initial"
  elseif action == "get_move"
    ["move" => [0,0]]
  elseif action == "exit"
    exit()
  end
end

function handle_input(data::Dict)
  theData = get(data, "__data__", None)
  uuid = get(data, "__uuid__", None)
  action = get(data, "__action__", None)

  result = handle_action(action, theData)
  ["__uuid__" => uuid, "__return__" => result]
end


while true
  msg = ZMQ.recv(socket)
  out = convert(IOStream, msg)
  seek(out,0)

  #read out::MemIO as usual, eg. read(out,...) or takebuf_string(out)
  #or, conveniently, use ASCIIString[msg] to retrieve a string

  json_input = JSON.parse(takebuf_string(out))

  ret = handle_input(json_input)

  json_output = JSON.json(ret)

  ZMQ.send(socket, ZMQ.Message(json_output))
end

ZMQ.close(socket)
