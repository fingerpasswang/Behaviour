using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThunkBT
{
    public static class Math
    {
        public static Thunk<float> Plus(Thunk<float> a, Thunk<float> b)
        {
            return Help.MakePureThunk(a.GetUserValue() + b.GetUserValue());
        }
    }
}
