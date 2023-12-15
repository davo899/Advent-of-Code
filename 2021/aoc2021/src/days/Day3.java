package days;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Day3 extends Day {

    public Day3(boolean example) {
        super(3, example);
    }

    @Override
    public String part1() {
        int gamma = 0;
        int epsilon = 0;
        for (int i = 0; i < input.get(0).length(); i++) {
            int ones = 0;
            int zeros = 0;
            for (String line : input) {
                if (line.charAt(i) == '1') ones++;
                else zeros++;
            }
            if (ones > zeros) gamma += Math.pow(2, input.get(0).length() - i - 1);
            else if (ones < zeros) epsilon += Math.pow(2, input.get(0).length() - i - 1);
        }
        return String.valueOf(gamma * epsilon);
    }

    @Override
    public String part2() {
        int oxygen;
        int co;
        {
            List<String> searchList = new ArrayList<>(input);
            for (int i = 0; searchList.size() != 1; i++) {
                int ones = 0;
                int zeros = 0;
                for (String line : searchList) {
                    if (line.charAt(i) == '1') ones++;
                    else zeros++;
                }
                int finalI = i;
                if (ones >= zeros) {
                    searchList = searchList.stream()
                        .filter((line) -> line.charAt(finalI) == '1')
                        .collect(Collectors.toList());
                } else {
                    searchList = searchList.stream()
                        .filter((line) -> line.charAt(finalI) == '0')
                        .collect(Collectors.toList());
                }
            }
            oxygen = Integer.parseInt(searchList.get(0), 2);
        }
        {
            List<String> searchList = new ArrayList<>(input);
            for (int i = 0; searchList.size() != 1; i++) {
                int ones = 0;
                int zeros = 0;
                for (String line : searchList) {
                    if (line.charAt(i) == '1') ones++;
                    else zeros++;
                }
                int finalI = i;
                if (ones >= zeros) {
                    searchList = searchList.stream()
                        .filter((line) -> line.charAt(finalI) == '0')
                        .collect(Collectors.toList());
                } else {
                    searchList = searchList.stream()
                        .filter((line) -> line.charAt(finalI) == '1')
                        .collect(Collectors.toList());
                }
            }
            co = Integer.parseInt(searchList.get(0), 2);
        }
        return String.valueOf(oxygen * co);
    }

}
