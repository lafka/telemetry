-module(bounded_queue).

-type max_size() :: non_neg_integer().
-type size() :: non_neg_integer().

-type queue_item() :: term().

-record(bounded_queue, {max_size :: max_size(),
                        size :: size(),
                        queue :: queue:queue(),
                        type = fifo :: lifo | fifo}).
%-type bounded_queue() :: {bounded_queue,
%                          MaxSize :: max_size(),
%                          QueueSize :: size(),
%                          Queue :: queue:queue()}.


-export([
     new/1
   , new/2
   , size/1
   , in/2
   , out/1
   , to_list/1
]).

-spec new(max_size()) -> #bounded_queue{}.
new(MaxSize) -> new(MaxSize, fifo).
new(MaxSize, Type) when MaxSize > 0, (Type =:= fifo orelse Type =:= lifo) ->
   #bounded_queue{max_size = MaxSize,
                  size = 0,
                  queue = queue:new(),
                  type = Type}.

-spec size(#bounded_queue{}) -> size().
size(#bounded_queue{size = Size}) -> Size.

-spec in(#bounded_queue{}, queue_item()) -> #bounded_queue{}.
in(#bounded_queue{max_size = MaxSize, size = MaxSize} = Q, Item) ->
   {ok, {_, Q2}} = out(Q),
   in(Q2, Item);

in(#bounded_queue{size = Size, queue = Queue, type = fifo} = Q, Item) ->
   NewQueue = queue:in(Item, Queue),
   Q#bounded_queue{size = Size + 1, queue = NewQueue};

in(#bounded_queue{size = Size, queue = Queue, type = lifo} = Q, Item) ->
   NewQueue = queue:in_r(Item, Queue),
   Q#bounded_queue{size = Size + 1, queue = NewQueue}.

-spec out(#bounded_queue{}) -> {ok, {queue_item(), #bounded_queue{}}}
                             | {error, {empty, #bounded_queue{}}}.
out(#bounded_queue{size = Size, queue = Queue, type = Type} = Q) ->
   case outimpl(Type, Queue) of
      {empty, Queue} ->
         {error, {empty, Q}};

      {{value, Out}, NewQueue} ->
         {ok, {Out, Q#bounded_queue{queue = NewQueue, size = Size - 1}}}
   end.

-spec to_list(#bounded_queue{}) -> [queue_item()].
to_list(#bounded_queue{queue = Queue}) -> queue:to_list(Queue).

outimpl(lifo, Queue) -> queue:out(Queue);
outimpl(fifo, Queue) -> queue:out(Queue).

%defmodule Buffer.Queue do
%  defstruct [:max_size, :size, :queue]
%
%  def new(max_size) when max_size > 0, do:
%    %__MODULE__{max_size: max_size, size: 0, queue: :queue.new()}
%
%  def size(buffer), do: buffer.size
%
%  def push(%__MODULE__{max_size: max_size, size: max_size} = buffer, item) do
%    {:ok, {_, buffer}} = pull(buffer)
%    push(buffer, item)
%  end
%  def push(buffer, item), do:
%    %__MODULE__{buffer | size: buffer.size + 1, queue: :queue.in(item, buffer.queue)}
%
%  def pull(buffer) do
%    case :queue.out(buffer.queue) do
%      {:empty, _} -> {:error, :empty}
%      {{:value, item}, queue} ->
%        {:ok, {item, %__MODULE__{buffer | size: buffer.size - 1, queue: queue}}}
%    end
%  end
%end
