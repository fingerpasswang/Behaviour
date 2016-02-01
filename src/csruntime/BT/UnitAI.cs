using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BT
{
    public class UnitAISets<T> 
        where T : IUnit
    {
        #region IsFleeing
        private class MessageIsFleeing : IO<bool>
        {
            public MessageIsFleeing() { }
            public bool Drive(Context ctx)
            {
                return ((T) ctx.Self).IsFleeing();
            }
        }

        public static IO<bool> IsFleeing()
        {
            return new MessageIsFleeing();
        }
        #endregion

        #region HpRateLessThan
        private class MessageHpRateLessThan : IO<bool>
        {
            public readonly float p0;

            public MessageHpRateLessThan(float p0)
            {
                this.p0 = p0;
            }

            public bool Drive(Context ctx)
            {
                return ((T)ctx.Self).HpRateLessThan(p0);
            }
        }

        public static IO<bool> HpRateLessThan(float p0)
        {
            return new MessageHpRateLessThan(p0);
        }
        #endregion
    }
}
