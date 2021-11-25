package GPU;

import com.aparapi.Kernel;

public class GpuOperations {

    static public int[] plus(int[] v1, int[] v2, int p) {
        int[] result = new int[v1.length];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int i = getGlobalId();
                result[i] = (v1[i] + v2[i]) % p;
            }
        };
        kernel.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernel.execute(v1.length);
        return result;
    }

    static public int[] minus(int[] v1, int[] v2, int p) {
        int[] result = new int[v1.length];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int i = getGlobalId();
                 int v = (v1[i] - v2[i]) % p;
                result[i] = (v < 0)? v + p : v;
            }
        };
        kernel.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernel.execute(v1.length);
        return result;
    }

    static public int[] prod(int[] v1, int v2, int p) {
        int[] result = new int[v1.length];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int i = getGlobalId();
                result[i] = (v1[i] * v2) % p;
            }
        };
        kernel.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernel.execute(v1.length);
        return result;
    }

}
