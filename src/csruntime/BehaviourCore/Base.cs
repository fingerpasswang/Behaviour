using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BehaviourCore
{
    public interface IO<T>
    {
        T Drive(ConstContext ctx);
    }

    public class ConstContext
    {
        
    }
}
