using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ComputingBT
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

    public interface Thunk
    {
        bool IsPure();
    }

    public abstract class Thunk<T> : Thunk
    {
        protected bool pure = true;
        public abstract T GetUserValue();

        public bool IsPure()
        {
            return pure;
        }
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
            foreach (var thunk in ts)
            {
                if (!thunk.IsPure())
                {
                    this.pure = false;
                    break;
                }
            }
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

    public delegate Thunk<TR> FuncThunk<T0, TR> (Thunk<T0>p0);

    public class Closure<T0, TR> : Thunk<Closure<T0, TR>>
    {
        class UserFuncApply : Thunk<TR>
        {
            private Closure<T0, TR> func;
            private Thunk<T0> p0;

            public UserFuncApply(Closure<T0, TR> func, Thunk<T0> p0)
            {
                this.func = func;
                this.p0 = p0;
                this.pure = false;
            }

            public override TR GetUserValue()
            {
                return func.funcThunk(p0).GetUserValue();
            }
        }

        private bool isUserFunc = false;
        private FuncThunk<T0, TR> funcThunk;
        private Func<T0, TR> userFunc; 

        public Closure(FuncThunk<T0, TR> funcThunk)
        {
            this.funcThunk = funcThunk;
        }

        public Closure(Func<T0, TR> func)
        {
            this.userFunc = func;
            this.funcThunk = p0 => Help.MakePureThunk(userFunc(p0.GetUserValue()));
            this.isUserFunc = true;
        }

        public override Closure<T0, TR> GetUserValue()
        {
            return this;
        }

        public Thunk<TR> Apply(Thunk<T0> p0)
        {
            if (!isUserFunc || Help.AllPure(p0))
            {
                return funcThunk(p0);
            }

            return new UserFuncApply(this, p0);
        }
    }
}
