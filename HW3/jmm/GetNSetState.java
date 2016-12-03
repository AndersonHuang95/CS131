import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private byte maxval;
    private AtomicIntegerArray value;
    
    GetNSetState(byte[] v) { 
        int[] tmp = new int[v.length];
        for (int i = 0; i < v.length; ++i) {
            tmp[i] = v[i];
        }
        value = new AtomicIntegerArray(tmp); 
        maxval = 127; 
        }

    GetNSetState(byte[] v, byte m) { 
        int[] tmp = new int[v.length];
        for (int i = 0; i < v.length; ++i) {
            tmp[i] = v[i];
        }
        value = new AtomicIntegerArray(tmp); 
        maxval = m; 
    }

    // Notice the length is now a method, not a member variable!
    public int size() { return value.length(); }

    public byte[] current() { 
        byte[] result = new byte[value.length()];
        for (int i = 0; i < value.length(); ++i) {
            result[i] = (byte)value.get(i);
        }
        return result;
    
    }

    public boolean swap(int i, int j) {
        if (value.get(i) <= 0 || value.get(j) >= maxval) {
            return false;
        }
        value.set(i, value.get(i) - 1); 
        value.set(j, value.get(j) + 1);
        return true;
    }
}