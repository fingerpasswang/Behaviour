using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThunkBT
{
    public enum Result
    {
        Continue,
        Success,
        Failure,
    }

    public class Continuation
    {
        public Continuation SubContinuation { get; set; }
        public int NextStep { get; set; }
        public object Param { get; set; }
    }

    public class Context
    {
        public Continuation Continuation { get; set; }
        public object Self { get; set; }
    }

    public abstract class IO<T> : Thunk<IO<T>>
    {
        public abstract T Drive(Context ctx);
        public override IO<T> GetUserValue()
        {
            return this;
        }
    }

    public abstract class Thunk<T>
    {
        public abstract T GetUserValue();
    }

    public class ThunkList<T> : Thunk<ThunkList<T>>
    {
        private readonly Thunk<T>[] arr;

        public T this[int i]
        {
            get
            {
                return arr[i].GetUserValue();
            }
        }

        public int Length
        {
            get
            {
                return arr.Length;
            }
        }

        public ThunkList(Thunk<T>[] ts)
        {
            this.arr = ts;
        }

        public override ThunkList<T> GetUserValue()
        {
            return this;
        }
    }

    public class PureThunk<T> : Thunk<T>
    {
        private readonly T val;

        public PureThunk(T val)
        {
            this.val = val;
        }

        public override T GetUserValue()
        {
            return val;
        }
    }
}
