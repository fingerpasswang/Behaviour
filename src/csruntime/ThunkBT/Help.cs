using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThunkBT
{
    public static class Help
    {
        public static Thunk<T> MakePureThunk<T>(T v)
        {
            return new PureThunk<T>(v);
        }

        public static ThunkList<T> MakeList<T>(params Thunk<T>[] ts)
        {
            return new ThunkList<T>(ts);
        }
    }
}
