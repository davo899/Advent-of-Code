package days;

import java.util.List;

public class Day1 extends Day {

    public Day1(boolean example) {
        super(1, example);
    }

    @Override
    public String part1() {
        int count = 0;
        int current = Integer.parseInt(input.get(0));
        for (int i = 1; i < input.size(); i++) {
            int next = Integer.parseInt(input.get(i));
            if (next > current) count++;
            current = next;
        }
        return count + "";
    }

    @Override
    public String part2() {
        List<Integer> ints = input.stream().map(Integer::parseInt).toList();
        int current = ints.get(0) + ints.get(1) + ints.get(2);
        int count = 0;
        for (int i = 2; i < ints.size() - 1; i++) {
            int next = ints.get(i - 1) + ints.get(i) + ints.get(i + 1);
            if (next > current) count++;
            current = next;
        }
        return count + "";
    }

}
