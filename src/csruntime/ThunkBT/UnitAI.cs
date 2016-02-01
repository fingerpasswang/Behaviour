using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThunkBT
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

        public static Thunk<IO<bool>> HpRateLessThan(Thunk<float> p0)
        {
            return new MessageHpRateLessThan(p0);
        }
        #endregion
    }
}
