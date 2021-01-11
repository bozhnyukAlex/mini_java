open Java_lib.Transform
open Format

let test_val =
  {|
  public class Main {
    public static void main(String[] args) {
        int[] arr = new int[] {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
        QuickSorter quickSorter = new QuickSorter();
        int n = 16;
        int i = 0;
        int j = 15;
        quickSorter.quickSort(arr, n, i, j);
    }
}

class QuickSorter {
    public void quickSort(int [] array, int n, int low, int high) {
        if (n == 0)
            return;
        if (low >= high)
            return;
        int middle = low + (high - low) / 2;
        int pivot = array[middle];
        int i = low, j = high;
        while (i <= j) {
            while (array[i] < pivot) {
                i++;
            }
            while (array[j] > pivot) {
                j--;
            }
            if (i <= j) {
                int temp = array[i];
                array[i] = array[j];
                array[j] = temp;
                i++;
                j--;
            }
        }
        if (low < j)
            quickSort(array, n, low, j);

        if (high > i)
            quickSort(array, n, i, high);
    }
}

public class Main {
    public static void main(String[] args) {
        int[] arr = new int[10];
        for (int i = 0; i < 10; i++) {
            if (i % 2 == 0) {
                continue;
            }
            arr[i] = 1;
        }
    }
}
|}

let () = transform_rename test_val "i" "k" std_formatter
