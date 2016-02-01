using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComputingBT
{
    public class UnitAISets<T>
        where T : IUnit
    {
        #region IsFleeing
        private class MessageIsFleeing : IO<bool>
        {
            public MessageIsFleeing() { }
            public override bool Drive(Context ctx)
            {
                return ((T)ctx.Self).IsFleeing();
            }
        }

        public static Thunk<IO<bool>> IsFleeing()
        {
            return new MessageIsFleeing();
        }
        #endregion

        #region HpRateLessThan
        private class MessageHpRateLessThan : IO<bool>
        {
            public readonly Thunk<float> p0;

            public MessageHpRateLessThan(Thunk<float> p0)
            {
                this.p0 = p0;
            }

            public override bool Drive(Context ctx)
            {
                return ((T)ctx.Self).HpRateLessThan(p0.GetUserValue());
            }
        }

        public static readonly Thunk<Closure<float, IO<bool>>> HpRateLessThan
            = Help.MakePureFuncThunk<float, IO<bool>>(p0 => new MessageHpRateLessThan(p0));

        #endregion

    }

    public class TestHelp
    {
        public void Test()
        {
            var box1 = new Box<float>();

            // Math.Abs(box1) -> UserFuncApply
            // 在GetUserValue的时候才会求值
            var ret1 = Help.Apply(Math.Abs, box1);

            // Math.Abs(0.2f) -> Thunk<float>
            // 直接构造出来了一个Thunk<float>(0.2f)
            var ret2 = Help.Apply(Math.Abs, Help.MakePureThunk(0.2f));

            // UnitAISets<IUnit>.HpRateLessThan(box1) -> Message
            var ret3 = Help.Apply(UnitAISets<IUnit>.HpRateLessThan, box1);

            // UnitAISets<IUnit>.HpRateLessThan(0.2f) -> Message
            var ret4 = Help.Apply(UnitAISets<IUnit>.HpRateLessThan, Help.MakePureThunk(0.2f));
        }
    }
}
