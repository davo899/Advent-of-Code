package days;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public abstract class Day {

    protected List<String> input = new ArrayList<>();

    protected Day(int n, boolean example) {
        if (n < 1 || n > 25) throw new IllegalArgumentException("Invalid day: " + n);

        try {
            File myObj = new File("resources/" + (example ? "example" : "day") + n + ".txt");
            Scanner reader = new Scanner(myObj);
            while (reader.hasNextLine()) input.add(reader.nextLine());
            reader.close();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public abstract String part1();

    public abstract String part2();

}
