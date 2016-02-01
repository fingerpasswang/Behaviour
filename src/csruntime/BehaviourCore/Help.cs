using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BehaviourCore
{
    public class Help
    {
        public static List<T> MakeList<T>(params T[] objs)
        {
            return new List<T>(objs);
        }
    }
}
