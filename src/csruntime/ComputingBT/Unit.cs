using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComputingBT
{

    public interface IUnit
    {
        bool IsFleeing();
        object GetNearestTarget();
        bool IsNull();
        Result Patrol();
        bool IsAttacking();
        float GetFleeBloodRate();
        bool HpRateLessThan(float rate);
        bool IsNormal();
        Result LockTarget(object target);
        bool ReachCurrentPatrolPoint();
        Result MoveToNextPatrolPoiont();
        Result Idle();
    }
}
