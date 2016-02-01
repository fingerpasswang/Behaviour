using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComputingBT
{
    public static class Help
    {
        public static Thunk<T> MakePureThunk<T>(T v)
        {
            return new PureThunk<T>(v);
        }

        public static Thunk<Closure<T0, TR>> MakePureFuncThunk<T0, TR>(FuncThunk<T0, TR> f)
        {
            return new Closure<T0, TR>(f);
        }

        public static Thunk<Closure<T0, TR>> MakeUserFuncThunk<T0, TR>(Func<T0, TR> f)
        {
            return new Closure<T0, TR>(f);
        }

        public static ThunkList<T> MakeList<T>(params Thunk<T>[] ts)
        {
            return new ThunkList<T>(ts);
        }

        public static bool AllPure(params Thunk[] ts)
        {
            if (ts == null) 
            {
				return true;
			}
			for (int i = 0; i<ts.Length; i++) 
            {
				if (!ts[i].IsPure()) 
                {
					return false;
				}
			}
			return true;
        }

        public static Thunk<TR> Apply<T0, TR>(Thunk<Closure<T0, TR>> func, Thunk<T0> p0)
        {
            return func.GetUserValue().Apply(p0);
        }
    }
}
