import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;
    private CountDownLatch doneSignal;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery, CountDownLatch l) {
        // TODO
        rnd = new Random();
        this.bakery = bakery;
        this.doneSignal = l;
        shoppingCart = new ArrayList<BreadType>(3);
        this.fillShoppingCart();
        this.shopTime = rnd.nextInt(33);
        this.checkoutTime = rnd.nextInt(100);
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        System.out.println("Started shopping: " + this.toString());
        try {
            // Customer starts shopping
            for (BreadType bread : shoppingCart) {
                // For each type of bread they want, they occupy the shelf
                bakery.shelves.get(bread).acquire();
                // Spend time "shopping" using the randomly generated shopTime
                Thread.sleep(shopTime);
                // They take the bread and the shelf is restocked if necessary
                bakery.takeBread(bread);
                // They leave the shelf and it becomes available again
                bakery.shelves.get(bread).release();
            }
            // After customer is done shopping, they
            // find (or wait) for a register
            bakery.registers.acquire();
            // Spend time "checking out" using the randomly generated checkoutTime
            Thread.sleep(checkoutTime);
            // "Checkout" by adding to global sales
            // Lock the sales variable to prevent race conditions
            bakery.salesMutex.acquire();
            bakery.addSales(getItemsValue());
            bakery.salesMutex.release();
            // Free the register
            bakery.registers.release();
            // Signal customer is done, and leaves the bakery
            // Frees a spot in the thread pool for next customer
            System.out.println("Done shopping: " + this.toString());
            doneSignal.countDown();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime="
                + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}
