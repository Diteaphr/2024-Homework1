// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;


/* Problem 1 Interface & Contract */
contract StudentV1 {
    uint256 private studentCode = 6969;
    uint256 count = 0; // 來記錄有register 是不是第一次被呼叫過
    function register() external returns (uint256) {
        if (count >= 1) { //如果第二次被呼叫的話才要傳正確的程式碼
            studentCode = 123; 
        }
        count += 1;
        return studentCode; 
    }
}

/* Problem 2 Interface & Contract */
interface IClassroomV2 {
    function isEnrolled() external view returns (bool);
}

contract StudentV2 {
    function register() external view returns (uint256) {
        // Accessing isEnrolled directly from ClassroomV2 contract
        if (IClassroomV2(msg.sender).isEnrolled()) {
            return 123; // If enrolled, return 123
        } else {
            return 6969; // If not enrolled, return 1001
        }
    }
}

/* Problem 3 Interface & Contract */
contract StudentV3 {
    uint256 left = 0;
    function register() external returns (uint256) {
        if (gasleft() >= 7000 || gasleft() <= 4000) {
            return 6969; // 第一次调用返回0
        } else {
            return 123;
        }
    }
}