defmodule BoundedQueueTest do
  use ExUnit.Case

  test "api, fifo" do
    q = :bounded_queue.new 10, :fifo

    q1 = :bounded_queue.in q, 1
    q2 = :bounded_queue.in q1, 2
    q3 = :bounded_queue.in q2, 3

    {:ok, {_, nq1}} = assert {:ok, {1, _}} = :bounded_queue.out(q3)
    {:ok, {_, nq2}} = assert {:ok, {2, _}} = :bounded_queue.out(nq1)
    {:ok, {_, nq3}} = assert {:ok, {3, _}} = :bounded_queue.out(nq2)
    assert {:error, {:empty, nq3}} = :bounded_queue.out(nq3)

    assert 0 === :bounded_queue.size(nq3)
    assert 1 === :bounded_queue.size(nq2)
    assert 2 === :bounded_queue.size(nq1)
  end

  test "api, lifo" do
    q = :bounded_queue.new 10, :lifo

    q1 = :bounded_queue.in q, 1
    q2 = :bounded_queue.in q1, 2
    q3 = :bounded_queue.in q2, 3

    {:ok, {_, nq1}} = assert {:ok, {3, _}} = :bounded_queue.out(q3)
    {:ok, {_, nq2}} = assert {:ok, {2, _}} = :bounded_queue.out(nq1)
    {:ok, {_, nq3}} = assert {:ok, {1, _}} = :bounded_queue.out(nq2)
    assert {:error, {:empty, nq3}} = :bounded_queue.out(nq3)

    assert 0 === :bounded_queue.size(nq3)
    assert 1 === :bounded_queue.size(nq2)
    assert 2 === :bounded_queue.size(nq1)
  end

  test "bounds check - fifo" do
    q = :bounded_queue.new 2, :fifo

    q1 = :bounded_queue.in q, 1
    q2 = :bounded_queue.in q1, 2

    assert [1, 2] = :bounded_queue.to_list q2

    q3 = :bounded_queue.in q2, 3
    assert [2, 3] = :bounded_queue.to_list q3

    q4 = :bounded_queue.in q3, 4
    assert [3, 4] = :bounded_queue.to_list q4
  end

  test "bounds check - lifo" do
    # lifo functions as a stack, only switches the first element until
    # someone pulls a  item
    q = :bounded_queue.new 2, :lifo

    q1 = :bounded_queue.in q, 1
    q2 = :bounded_queue.in q1, 2

    assert [2, 1] = :bounded_queue.to_list q2

    q3 = :bounded_queue.in q2, 3
    assert [3, 1] = :bounded_queue.to_list q3

    q4 = :bounded_queue.in q3, 4
    assert [4, 1] = :bounded_queue.to_list q4
  end
end
