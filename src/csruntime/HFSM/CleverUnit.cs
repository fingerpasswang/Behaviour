using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HFSM
{
    public interface ICleverUnit
    {
        void Patrol(); // 巡逻
        ICleverUnit GetNearestTarget();
        void LockTarget(ICleverUnit unit);
        float GetFleeBloodRate();
        bool CanMove();
        void MoveToNextPatrolPoiont();
        bool ReachCurrentPatrolPoint();
        bool HpRateLessThan(float rate);
        void Flee();
        void Speak();
        void Idle();
    }
}
