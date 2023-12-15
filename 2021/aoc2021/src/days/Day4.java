package days;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day4 extends Day {

    public Day4(boolean example) {
        super(4, example);
    }

    List<List<Integer>> boards;
    {
        boards = new ArrayList<>();
        for (int i = 2; i < input.size(); i += 6) {
            List<Integer> board = new ArrayList<>();
            for (int j = 0; j < 5; j++) {
                Arrays.stream(input.get(i + j).split(" "))
                    .filter((word) -> !word.isEmpty())
                    .map(Integer::parseInt)
                    .forEach(board::add);
            }
            boards.add(board);
        }
    }

    @Override
    public String part1() {
        Queue<Integer> calls = Arrays.stream(input.get(0).split(","))
            .map(Integer::parseInt)
            .collect(Collectors.toCollection(ArrayDeque::new));
        int last = 0;
        Set<Integer> called = new HashSet<>();
        Optional<List<Integer>> winner;
        do {
            last = calls.poll();
            called.add(last);
            winner = boards.stream().filter(
                (numbers) ->
                    called.containsAll(numbers.subList(0, 5)) ||
                    called.containsAll(numbers.subList(5, 10)) ||
                    called.containsAll(numbers.subList(10, 15)) ||
                    called.containsAll(numbers.subList(15, 20)) ||
                    called.containsAll(numbers.subList(20, 25)) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 0).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 1).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 2).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 3).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 4).mapToObj(numbers::get).toList())
            ).findAny();
        } while(winner.isEmpty());

        AtomicInteger unmarked = new AtomicInteger();
        winner.get().stream().filter((n) -> !called.contains(n)).forEach(unmarked::addAndGet);
        return String.valueOf(unmarked.get() * last);
    }

    @Override
    public String part2() {
        Queue<Integer> calls = Arrays.stream(input.get(0).split(","))
            .map(Integer::parseInt)
            .collect(Collectors.toCollection(ArrayDeque::new));
        int last;
        List<Integer> lastBoard;
        Set<Integer> called = new HashSet<>();
        List<List<Integer>> boards_ = new ArrayList<>(boards);
        do {
            last = calls.poll();
            called.add(last);
            lastBoard = boards_.get(0);
            boards_ = boards_.stream().filter(
                (numbers) ->
                    !(called.containsAll(numbers.subList(0, 5)) ||
                    called.containsAll(numbers.subList(5, 10)) ||
                    called.containsAll(numbers.subList(10, 15)) ||
                    called.containsAll(numbers.subList(15, 20)) ||
                    called.containsAll(numbers.subList(20, 25)) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 0).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 1).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 2).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 3).mapToObj(numbers::get).toList()) ||
                    called.containsAll(IntStream.range(0, 25).filter((i) -> i % 5 == 4).mapToObj(numbers::get).toList()))
            ).toList();
        } while(boards_.size() > 0);

        AtomicInteger unmarked = new AtomicInteger();
        lastBoard.stream().filter((n) -> !called.contains(n)).forEach(unmarked::addAndGet);
        return String.valueOf(unmarked.get() * last);
    }

}
