using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace StateMachine
{
    public interface IUnit
    {
        void ChangeState(UnitStateEnum state);
        void Patrol(); 
        IUnit GetNearestTarget(); 
        void LockTarget(IUnit unit);
        float GetFleeBloodRate();
        bool CanMove();
        bool HpRateLessThan(float rate);
        void Flee();
        void Speak();
    }

}
