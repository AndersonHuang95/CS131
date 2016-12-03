import java.util.concurrent.locks.ReentrantLock;

class BetterSafeState implements State {
	private byte maxval;
    private byte[] value;

    private final ReentrantLock reentrant_lock = new ReentrantLock(); 

    BetterSafeState(byte[] v) { value = v; maxval = 127; }

    BetterSafeState(byte[] v, byte m) { value = v; maxval = m; }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
	    reentrant_lock.lock(); 
		if (value[i] <= 0 || value[j] >= maxval) {
			reentrant_lock.unlock(); 
		    return false;
		}
		value[i]--;
		value[j]++;
		reentrant_lock.unlock();
		return true;
	}
}
