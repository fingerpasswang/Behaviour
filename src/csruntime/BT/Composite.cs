using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BT
{
    public class Sequence : IO<Result>
    {
        private readonly ICollection<IO<Result>> subTrees;
        public Sequence(ICollection<IO<Result>> subTrees)
        {
            this.subTrees = subTrees;
        }
        public Result Drive(Context ctx)
        {
            throw new NotImplementedException();
        }
    }

    public class Select : IO<Result>
    {
        private readonly ICollection<IO<Result>> subTrees;
        public Select(ICollection<IO<Result>> subTrees)
        {
            this.subTrees = subTrees;
        }

        public Result Drive(Context ctx)
        {
            throw new NotImplementedException();
        }
    }
}
