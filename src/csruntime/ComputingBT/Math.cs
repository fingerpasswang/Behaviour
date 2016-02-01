using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComputingBT
{
    public static class Math
    {
        public static readonly Thunk<Closure<float, float>> Abs = Help.MakeUserFuncThunk<float,float>(System.Math.Abs);
    }
}
