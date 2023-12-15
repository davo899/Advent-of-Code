package days;

import java.util.List;

public class Day2 extends Day {

    public Day2(boolean example) {
        super(2, example);
    }

    @Override
    public String part1() {
        int x = 0;
        int y = 0;
        for (String line : input) {
            String[] words = line.split(" ");
            int n = Integer.parseInt(words[1]);
            switch (words[0]) {
                case "forward" -> x += n;
                case "up" -> y -= n;
                case "down" -> y += n;
            }
        }
        return String.valueOf(x * y);
    }

    @Override
    public String part2() {
        int x = 0;
        int y = 0;
        int aim = 0;
        for (String line : input) {
            String[] words = line.split(" ");
            int n = Integer.parseInt(words[1]);
            switch (words[0]) {
                case "forward" -> {
                    x += n;
                    y += aim * n;
                }
                case "up" -> aim -= n;
                case "down" -> aim += n;
            }
        }
        return String.valueOf(x * y);
    }

}
