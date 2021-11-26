package GPU;

import com.aparapi.Kernel;

public class GpuOperations2 {
    int[] result_ = new int[1];
    int[] v1_ = new int[1];
    int[] v2_ = new int[1];
    int v_ = 1;
    int p_ = 3;
    Kernel kernelPlus = new Kernel() {
        @Override
        public void run() {
            int i = getGlobalId();
            result_[i] = (v1_[i] + v2_[i]) % p_;
        }
    };

    Kernel kernelMinus = new Kernel() {
        @Override
        public void run() {
            int i = getGlobalId();
            int v = (v1_[i] - v2_[i]) % p_;
            result_[i] = (v < 0) ? v + p_ : v;
        }
    };

    Kernel kernelProd = new Kernel() {
        @Override
        public void run() {
            int i = getGlobalId();
            result_[i] = (v1_[i] * v_) % p_;
        }
    };

    public int[] plus(int[] v1, int[] v2, int p) {
        result_ = new int[v1.length];
        v1_ = v1;
        v2_ = v2;
        p_ = p;
        kernelProd.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernelPlus.execute(v1.length);
        kernelPlus.dispose();
        return result_;
    }

    public int[] minus(int[] v1, int[] v2, int p) {
        result_ = new int[v1.length];
        v1_ = v1;
        v2_ = v2;
        p_ = p;
        kernelMinus.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernelMinus.execute(v1.length);
        kernelMinus.dispose();
        return result_;
    }

    public int[] prod(int[] v1, int v2, int p) {
        result_ = new int[v1.length];
        v1_ = v1;
        v_ = v2;
        p_ = p;
        kernelProd.setExecutionMode(Kernel.EXECUTION_MODE.GPU);
        kernelProd.execute(v1.length);
        kernelProd.dispose();
        return result_;
    }
}
