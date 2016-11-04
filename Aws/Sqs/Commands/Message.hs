module Aws.Sqs.Commands.Message
(
-- * User Message Attributes
  UserMessageAttributeCustomType
, UserMessageAttributeValue(..)
, UserMessageAttributeName
, UserMessageAttribute

-- * Send Message
, SendMessage(..)
, SendMessageResponse(..)

-- * Delete Message
, DeleteMessage(..)
, DeleteMessageResponse(..)

-- * Receive Message
, Message(..)
, ReceiveMessage(..)
, ReceiveMessageResponse(..)

-- * Change Message Visiblity
, ChangeMessageVisibility(..)
, ChangeMessageVisibilityResponse(..)
) where

import Aws.Core
import Aws.Sqs.Core
import Control.Applicative
import Control.Monad.Trans.Resource (throwM)
import Data.Maybe
import Data.Monoid
import Text.XML.Cursor (($/), ($//), (&/), (&|))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Scientific
import qualified Network.HTTP.Types as HTTP
import Text.Read (readEither)
import qualified Text.XML.Cursor as Cu
import Prelude

-- -------------------------------------------------------------------------- --
-- User Message Attributes

-- | You can append a custom type label to the supported data types (String,
-- Number, and Binary) to create custom data types. This capability is similar
-- to type traits in programming languages. For example, if you have an
-- application that needs to know which type of number is being sent in the
-- message, then you could create custom types similar to the following:
-- Number.byte, Number.short, Number.int, and Number.float. Another example
-- using the binary data type is to use Binary.gif and Binary.png to
-- distinguish among different image file types in a message or batch of
-- messages. The appended data is optional and opaque to Amazon SQS, which
-- means that the appended data is not interpreted, validated, or used by
-- Amazon SQS. The Custom Type extension has the same restrictions on allowed
-- characters as the message body.
--
type UserMessageAttributeCustomType = T.Text

-- | Message Attribute Value
--
-- The user-specified message attribute value. For string data types, the value
-- attribute has the same restrictions on the content as the message body. For
-- more information, see SendMessage.
--
-- Name, type, and value must not be empty or null. In addition, the message
-- body should not be empty or null. All parts of the message attribute,
-- including name, type, and value, are included in the message size
-- restriction, which is currently 256 KB (262,144 bytes).
--
-- The supported message attribute data types are String, Number, and Binary.
-- You can also provide custom information on the type. The data type has the
-- same restrictions on the content as the message body. The data type is case
-- sensitive, and it can be up to 256 bytes long.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_MessageAttributeValue.html>
--
data UserMessageAttributeValue
    = UserMessageAttributeString (Maybe UserMessageAttributeCustomType) T.Text
    -- ^ Strings are Unicode with UTF-8 binary encoding.

    | UserMessageAttributeNumber (Maybe UserMessageAttributeCustomType) Scientific
    -- ^ Numbers are positive or negative integers or floating point numbers.
    -- Numbers have sufficient range and precision to encompass most of the
    -- possible values that integers, floats, and doubles typically support. A
    -- number can have up to 38 digits of precision, and it can be between
    -- 10^-128 to 10^+126. Leading and trailing zeroes are trimmed.

    | UserMessageAttributeBinary (Maybe UserMessageAttributeCustomType) B.ByteString
    -- ^ Binary type attributes can store any binary data, for example,
    -- compressed data, encrypted data, or images.

    -- UserMessageAttributesStringList (Maybe UserMessageAttributeCustomType) [T.Text]
    -- -- ^ Not implemented. Reserved for future use.

    -- UserMessageAttributeBinaryList (Maybe UserMessageAttributeCustomType) [B.ByteString]
    -- -- ^ Not implemented. Reserved for future use.

    deriving (Show, Read, Eq, Ord)

-- | The message attribute name can contain the following characters: A-Z, a-z,
-- 0-9, underscore(_), hyphen(-), and period (.). The name must not start or
-- end with a period, and it should not have successive periods. The name is
-- case sensitive and must be unique among all attribute names for the message.
-- The name can be up to 256 characters long. The name cannot start with "AWS."
-- or "Amazon." (or any variations in casing) because these prefixes are
-- reserved for use by Amazon Web Services.
--
type UserMessageAttributeName = T.Text

-- | Message Attribute
--
-- Name, type, and value must not be empty or null. In addition, the message
-- body should not be empty or null. All parts of the message attribute,
-- including name, type, and value, are included in the message size
-- restriction, which is currently 256 KB (262,144 bytes).
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributes.DataTypes>
--
-- /NOTE/
--
-- The Amazon SQS API reference calls this /MessageAttribute/. The Haskell
-- bindings use this term for what the Amazon documentation calls just
-- /Attributes/. In order to limit backward compatibility issues we keep the
-- terminology of the Haskell bindings and call this type
-- /UserMessageAttributes/.
--
type UserMessageAttribute = (UserMessageAttributeName, UserMessageAttributeValue)

userMessageAttributesQuery :: [UserMessageAttribute] -> HTTP.Query
userMessageAttributesQuery = concat . zipWith msgAttrQuery [1 :: Int ..]
  where
    msgAttrQuery i (name, value) =
        [ ( pre <> ".Name", Just $ TE.encodeUtf8 name )
        , ( pre <> ".Value.DataType", Just typ )
        , ( pre <> ".Value." <> valueKey, Just encodedValue )
        ]
      where
        pre = "MessageAttribute." <> B.pack (show i) <> "."
        customType Nothing t = TE.encodeUtf8 t
        customType (Just c) t = TE.encodeUtf8 $ t <> "." <> c
        (typ, valueKey, encodedValue) = case value of
            UserMessageAttributeString c t ->
                (customType c "String", "StringValue", TE.encodeUtf8 t)
            UserMessageAttributeNumber c n ->
                (customType c "Number", "StringValue", B.pack $ show n)
            UserMessageAttributeBinary  c b ->
                (customType c "Binary", "BinaryValue", b)

-- -------------------------------------------------------------------------- --
-- Send Message

-- | Delivers a message to the specified queue. With Amazon SQS, you now have
-- the ability to send large payload messages that are up to 256KB (262,144
-- bytes) in size. To send large payloads, you must use an AWS SDK that
-- supports SigV4 signing. To verify whether SigV4 is supported for an AWS SDK,
-- check the SDK release notes.
--
-- /IMPORTANT/
--
-- The following list shows the characters (in Unicode) allowed in your
-- message, according to the W3C XML specification. For more information, go to
-- <http://www.w3.org/TR/REC-xml/#charsets> If you send any characters not
-- included in the list, your request will be rejected.
--
-- > #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_SendMessage.html>
--
data SendMessage = SendMessage
    { smMessage :: !T.Text
    -- ^ The message to send. String maximum 256 KB in size.

    , smQueueName :: !QueueName
    -- ^ The URL of the Amazon SQS queue to take action on.

    , smAttributes :: ![UserMessageAttribute]
    -- ^ Each message attribute consists of a Name, Type, and Value.

    , smDelaySeconds :: !(Maybe Int)
    -- ^ The number of seconds (0 to 900 - 15 minutes) to delay a specific
    -- message. Messages with a positive DelaySeconds value become available for
    -- processing after the delay time is finished. If you don't specify a value,
    -- the default value for the queue applies.
    }
    deriving (Show, Read, Eq, Ord)

-- | At
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_SendMessageResult.html>
-- all fields of @SendMessageResult@ are denoted as optional.
-- At
-- <http://queue.amazonaws.com/doc/2012-11-05/QueueService.wsdl>
-- all fields are specified as required.
--
-- The actual service seems to treat at least 'smrMD5OfMessageAttributes'
-- as optional.
--
data SendMessageResponse = SendMessageResponse
    { smrMD5OfMessageBody :: !T.Text
    -- ^ An MD5 digest of the non-URL-encoded message body string. This can be
    -- used to verify that Amazon SQS received the message correctly. Amazon SQS
    -- first URL decodes the message before creating the MD5 digest. For
    -- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.

    , smrMessageId :: !MessageId
    -- ^ An element containing the message ID of the message sent to the queue.

    , smrMD5OfMessageAttributes :: !(Maybe T.Text)
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. This can
    -- be used to verify that Amazon SQS received the message correctly. Amazon
    -- SQS first URL decodes the message before creating the MD5 digest. For
    -- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
    }
    deriving (Show, Read, Eq, Ord)

instance ResponseConsumer r SendMessageResponse where
    type ResponseMetadata SendMessageResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
      where
        parse el = SendMessageResponse
            <$> force "Missing MD5 Signature"
                (el $// Cu.laxElement "MD5OfMessageBody" &/ Cu.content)
            <*> (fmap MessageId . force "Missing Message Id")
                (el $// Cu.laxElement "MessageId" &/ Cu.content)
            <*> (pure . listToMaybe)
                (el $// Cu.laxElement "MD5OfMessageAttributes" &/ Cu.content)

instance SignQuery SendMessage where
    type ServiceConfiguration SendMessage = SqsConfiguration
    signQuery SendMessage{..} = sqsSignQuery SqsQuery
        { sqsQueueName = Just smQueueName
        , sqsQuery =
            [ ("Action", Just "SendMessage")
            , ("MessageBody", Just $ TE.encodeUtf8 smMessage)
            ]
            <> userMessageAttributesQuery smAttributes
            <> maybeToList (("DelaySeconds",) . Just . B.pack . show <$> smDelaySeconds)
        }

instance Transaction SendMessage SendMessageResponse

instance AsMemoryResponse SendMessageResponse where
    type MemoryResponse SendMessageResponse = SendMessageResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Delete Message

-- | Deletes the specified message from the specified queue. You specify the
-- message by using the message's receipt handle and not the message ID you
-- received when you sent the message. Even if the message is locked by another
-- reader due to the visibility timeout setting, it is still deleted from the
-- queue. If you leave a message in the queue for longer than the queue's
-- configured retention period, Amazon SQS automatically deletes it.
--
-- /NOTE/
--
-- The receipt handle is associated with a specific instance of receiving the
-- message. If you receive a message more than once, the receipt handle you get
-- each time you receive the message is different. When you request
-- DeleteMessage, if you don't provide the most recently received receipt
-- handle for the message, the request will still succeed, but the message
-- might not be deleted.
--
-- /IMPORTANT/
--
-- It is possible you will receive a message even after you have deleted it.
-- This might happen on rare occasions if one of the servers storing a copy of
-- the message is unavailable when you request to delete the message. The copy
-- remains on the server and might be returned to you again on a subsequent
-- receive request. You should create your system to be idempotent so that
-- receiving a particular message more than once is not a problem.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_DeleteMessage.html>
--
data DeleteMessage = DeleteMessage
    { dmReceiptHandle :: !ReceiptHandle
    -- ^ The receipt handle associated with the message to delete.
    , dmQueueName :: !QueueName
    -- ^ The URL of the Amazon SQS queue to take action on.
    }
    deriving (Show, Read, Eq, Ord)

data DeleteMessageResponse = DeleteMessageResponse {}
    deriving (Show, Read, Eq, Ord)

instance ResponseConsumer r DeleteMessageResponse where
    type ResponseMetadata DeleteMessageResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
      where
        parse _ = return DeleteMessageResponse {}

instance SignQuery DeleteMessage  where
    type ServiceConfiguration DeleteMessage = SqsConfiguration
    signQuery DeleteMessage{..} = sqsSignQuery SqsQuery
        { sqsQueueName = Just dmQueueName
        , sqsQuery =
            [ ("Action", Just "DeleteMessage")
            , ("ReceiptHandle", Just $ TE.encodeUtf8 $ printReceiptHandle dmReceiptHandle)
            ]
        }

instance Transaction DeleteMessage DeleteMessageResponse

instance AsMemoryResponse DeleteMessageResponse where
    type MemoryResponse DeleteMessageResponse = DeleteMessageResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Receive Message

-- | Retrieves one or more messages, with a maximum limit of 10 messages, from
-- the specified queue. Long poll support is enabled by using the
-- WaitTimeSeconds parameter. For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html Amazon SQS Long Poll>
-- in the Amazon SQS Developer Guide.
--
-- Short poll is the default behavior where a weighted random set of machines
-- is sampled on a ReceiveMessage call. This means only the messages on the
-- sampled machines are returned. If the number of messages in the queue is
-- small (less than 1000), it is likely you will get fewer messages than you
-- requested per ReceiveMessage call. If the number of messages in the queue is
-- extremely small, you might not receive any messages in a particular
-- ReceiveMessage response; in which case you should repeat the request.
--
-- For each message returned, the response includes the following:
--
-- Message body
--
-- * MD5 digest of the message body. For information about MD5, go to
--   <http://www.faqs.org/rfcs/rfc1321.html>.
--
-- * Message ID you received when you sent the message to the queue.
--
-- * Receipt handle.
--
-- * Message attributes.
--
-- * MD5 digest of the message attributes.
--
-- The receipt handle is the identifier you must provide when deleting the
-- message. For more information, see Queue and Message Identifiers in the
-- Amazon SQS Developer Guide.
--
-- You can provide the VisibilityTimeout parameter in your request, which will
-- be applied to the messages that Amazon SQS returns in the response. If you
-- do not include the parameter, the overall visibility timeout for the queue
-- is used for the returned messages. For more information, see Visibility
-- Timeout in the Amazon SQS Developer Guide.
--
-- /NOTE/
--
-- Going forward, new attributes might be added. If you are writing code that
-- calls this action, we recommend that you structure your code so that it can
-- handle new attributes gracefully.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_ReceiveMessage.html>
--
data ReceiveMessage = ReceiveMessage
    { rmVisibilityTimeout :: !(Maybe Int)
    -- ^ The duration (in seconds) that the received messages are hidden from
    -- subsequent retrieve requests after being retrieved by a ReceiveMessage
    -- request.

    , rmAttributes :: ![MessageAttribute]
    -- ^ A list of attributes that need to be returned along with each message.
    --
    -- The following lists the names and descriptions of the attributes that can
    -- be returned:
    --
    -- * All - returns all values.
    --
    -- * ApproximateFirstReceiveTimestamp - returns the time when the message was
    --   first received (epoch time in milliseconds).
    --
    -- * ApproximateReceiveCount - returns the number of times a message has been
    --   received but not deleted.
    --
    -- * SenderId - returns the AWS account number (or the IP address, if
    --   anonymous access is allowed) of the sender.
    --
    -- * SentTimestamp - returns the time when the message was sent (epoch time
    --   in milliseconds).

    , rmMaxNumberOfMessages :: !(Maybe Int)
    -- ^ The maximum number of messages to return. Amazon SQS never returns more
    -- messages than this value but may return fewer. Values can be from 1 to 10.
    -- Default is 1.
    --
    -- All of the messages are not necessarily returned.

    , rmUserMessageAttributes :: ![UserMessageAttributeName]
    -- ^ The name of the message attribute, where N is the index. The message
    -- attribute name can contain the following characters: A-Z, a-z, 0-9,
    -- underscore (_), hyphen (-), and period (.). The name must not start or end
    -- with a period, and it should not have successive periods. The name is case
    -- sensitive and must be unique among all attribute names for the message.
    -- The name can be up to 256 characters long. The name cannot start with
    -- "AWS." or "Amazon." (or any variations in casing), because these prefixes
    -- are reserved for use by Amazon Web Services.
    --
    -- When using ReceiveMessage, you can send a list of attribute names to
    -- receive, or you can return all of the attributes by specifying "All" or
    -- ".*" in your request. You can also use "foo.*" to return all message
    -- attributes starting with the "foo" prefix.

    , rmQueueName :: !QueueName
    -- ^The URL of the Amazon SQS queue to take action on.

    , rmWaitTimeSeconds :: !(Maybe Int)
    -- ^ The duration (in seconds) for which the call will wait for a message to
    -- arrive in the queue before returning. If a message is available, the call
    -- will return sooner than WaitTimeSeconds.

    }
    deriving (Show, Read, Eq, Ord)

-- | An Amazon SQS message.
--
-- In
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_Message.html>
-- all elements are denoted as optional.
-- In
-- <http://queue.amazonaws.com/doc/2012-11-05/QueueService.wsdl>
-- all elements except for the attributes are specified as required.
-- At least for the field 'mMD5OfMessageAttributes' the the service
-- is not always returning a value and therefor we make this field optional.
--
data Message = Message
    { mMessageId :: !T.Text
    -- ^ A unique identifier for the message. Message IDs are considered unique
    -- across all AWS accounts for an extended period of time.

    , mReceiptHandle :: !ReceiptHandle
    -- ^ An identifier associated with the act of receiving the message. A new
    -- receipt handle is returned every time you receive a message. When deleting
    -- a message, you provide the last received receipt handle to delete the
    -- message.

    , mMD5OfBody :: !T.Text
    -- ^ An MD5 digest of the non-URL-encoded message body string.

    , mBody :: T.Text
    -- ^ The message's contents (not URL-encoded).

    , mAttributes :: ![(MessageAttribute,T.Text)]
    -- ^ SenderId, SentTimestamp, ApproximateReceiveCount, and/or
    -- ApproximateFirstReceiveTimestamp. SentTimestamp and
    -- ApproximateFirstReceiveTimestamp are each returned as an integer
    -- representing the epoch time in milliseconds.

    , mMD5OfMessageAttributes :: !(Maybe T.Text)
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. This can
    -- be used to verify that Amazon SQS received the message correctly. Amazon
    -- SQS first URL decodes the message before creating the MD5 digest. For
    -- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.

    , mUserMessageAttributes :: ![UserMessageAttribute]
    -- ^ Each message attribute consists of a Name, Type, and Value.
    }
    deriving(Show, Read, Eq, Ord)

data ReceiveMessageResponse = ReceiveMessageResponse
    { rmrMessages :: ![Message]
    }
    deriving (Show, Read, Eq, Ord)

readMessageAttribute
    :: Cu.Cursor
    -> Response SqsMetadata (MessageAttribute,T.Text)
readMessageAttribute cursor = do
    name <- force "Missing Name" $ cursor $/ Cu.laxElement "Name" &/ Cu.content
    value <- force "Missing Value" $ cursor $/ Cu.laxElement "Value" &/ Cu.content
    parsedName <- parseMessageAttribute name
    return (parsedName, value)

readUserMessageAttribute
    :: Cu.Cursor
    -> Response SqsMetadata UserMessageAttribute
readUserMessageAttribute cursor = (,)
    <$> force "Missing Name" (cursor $/ Cu.laxElement "Name" &/ Cu.content)
    <*> readUserMessageAttributeValue cursor

readUserMessageAttributeValue
    :: Cu.Cursor
    -> Response SqsMetadata UserMessageAttributeValue
readUserMessageAttributeValue cursor = do
    typStr <- force "Missing DataType"
        $ cursor $/ Cu.laxElement "DataType" &/ Cu.content
    case parseType typStr of
        ("String", c) -> do
            val <- force "Missing StringValue"
                $ cursor $/ Cu.laxElement "StringValue" &/ Cu.content
            return $ UserMessageAttributeString c val

        ("Number", c) -> do
            valStr <- force "Missing StringValue"
                $ cursor $/ Cu.laxElement "StringValue" &/ Cu.content
            val <- tryXml . readEither $ T.unpack valStr
            return $ UserMessageAttributeNumber c val

        ("Binary", c) -> do
            val64 <- force "Missing StringValue"
                $ cursor $/ Cu.laxElement "StringValue" &/ Cu.content
            val <- tryXml . B64.decode $ TE.encodeUtf8 val64
            return $ UserMessageAttributeBinary c val

        (x, _) -> throwM . XmlException
            $ "unkown data type for MessageAttributeValue: " <> T.unpack x
  where
    parseType s = case T.break (== '.') s of
        (a, "") -> (a, Nothing)
        (a, x) -> (a, Just (T.tail x))
    tryXml = either (throwM . XmlException) return

readMessage :: Cu.Cursor -> Response SqsMetadata Message
readMessage cursor = do
    mid <- force "Missing Message Id"
        $ cursor $// Cu.laxElement "MessageId" &/ Cu.content
    rh <- force "Missing Reciept Handle"
        $ cursor $// Cu.laxElement "ReceiptHandle" &/ Cu.content
    md5 <- force "Missing MD5 Signature"
        $ cursor $// Cu.laxElement "MD5OfBody" &/ Cu.content
    body <- force "Missing Body"
        $ cursor $// Cu.laxElement "Body" &/ Cu.content
    attributes <- sequence
        $ cursor $// Cu.laxElement "Attribute" &| readMessageAttribute
    userAttributes <- sequence
        $ cursor $// Cu.laxElement "MessageAttribute" &| readUserMessageAttribute
    let md5OfMessageAttributes = listToMaybe
            $ cursor $// Cu.laxElement "MD5OfMessageAttributes" &/ Cu.content

    return Message
        { mMessageId = mid
        , mReceiptHandle = ReceiptHandle rh
        , mMD5OfBody = md5
        , mBody = body
        , mAttributes = attributes
        , mMD5OfMessageAttributes = md5OfMessageAttributes
        , mUserMessageAttributes = userAttributes
        }

formatMAttributes :: [MessageAttribute] -> HTTP.Query
formatMAttributes attrs = case attrs of
    [attr] -> [("AttributeName", encodeAttr attr)]
    _ -> zipWith f [1 :: Int ..] attrs
  where
    f x y = ("AttributeName." <> B.pack (show x), encodeAttr y)
    encodeAttr = Just . TE.encodeUtf8 . printMessageAttribute

formatUserMessageAttributes :: [UserMessageAttributeName] -> HTTP.Query
formatUserMessageAttributes attrs = case attrs of
    [attr] -> [("MessageAttributeName", encodeAttr attr)]
    _ -> zipWith f [1 :: Int ..] attrs
  where
    f x y = ("MessageAttributeName." <> B.pack (show x), encodeAttr y)
    encodeAttr = Just . TE.encodeUtf8

instance ResponseConsumer r ReceiveMessageResponse where
    type ResponseMetadata ReceiveMessageResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
      where
        parse el = do
            result <- force "Missing ReceiveMessageResult"
                $ el $// Cu.laxElement "ReceiveMessageResult"
            messages <- sequence
                $ result $// Cu.laxElement "Message" &| readMessage
            return ReceiveMessageResponse{ rmrMessages = messages }

instance SignQuery ReceiveMessage  where
    type ServiceConfiguration ReceiveMessage  = SqsConfiguration
    signQuery ReceiveMessage{..} = sqsSignQuery SqsQuery
        { sqsQueueName = Just rmQueueName
        , sqsQuery = [ ("Action", Just "ReceiveMessage") ]
            <> catMaybes
                [ ("VisibilityTimeout",) <$> case rmVisibilityTimeout of
                    Just x -> Just $ Just $ B.pack $ show x
                    Nothing -> Nothing

                , ("MaxNumberOfMessages",) <$> case rmMaxNumberOfMessages of
                    Just x -> Just $ Just $ B.pack $ show x
                    Nothing -> Nothing

                , ("WaitTimeSeconds",) <$> case rmWaitTimeSeconds of
                    Just x -> Just $ Just $ B.pack $ show x
                    Nothing -> Nothing
                ]
                <> formatMAttributes rmAttributes
                <> formatUserMessageAttributes rmUserMessageAttributes
        }

instance Transaction ReceiveMessage ReceiveMessageResponse

instance AsMemoryResponse ReceiveMessageResponse where
    type MemoryResponse ReceiveMessageResponse = ReceiveMessageResponse
    loadToMemory = return

-- -------------------------------------------------------------------------- --
-- Change Message Visibility

-- | Changes the visibility timeout of a specified message in a queue to a new
-- value. The maximum allowed timeout value you can set the value to is 12
-- hours. This means you can't extend the timeout of a message in an existing
-- queue to more than a total visibility timeout of 12 hours. (For more
-- information visibility timeout, see Visibility Timeout in the Amazon SQS
-- Developer Guide.)
--
-- For example, let's say you have a message and its default message visibility
-- timeout is 30 minutes. You could call ChangeMessageVisiblity with a value of
-- two hours and the effective timeout would be two hours and 30 minutes. When
-- that time comes near you could again extend the time out by calling
-- ChangeMessageVisiblity, but this time the maximum allowed timeout would be 9
-- hours and 30 minutes.
--
-- /NOTE/
--
-- There is a 120,000 limit for the number of inflight messages per queue.
-- Messages are inflight after they have been received from the queue by a
-- consuming component, but have not yet been deleted from the queue. If you
-- reach the 120,000 limit, you will receive an OverLimit error message from
-- Amazon SQS. To help avoid reaching the limit, you should delete the messages
-- from the queue after they have been processed. You can also increase the
-- number of queues you use to process the messages.
--
-- /IMPORTANT/
--
-- If you attempt to set the VisibilityTimeout to an amount more than the
-- maximum time left, Amazon SQS returns an error. It will not automatically
-- recalculate and increase the timeout to the maximum time remaining.
--
-- /IMPORTANT/
--
-- Unlike with a queue, when you change the visibility timeout for a specific
-- message, that timeout value is applied immediately but is not saved in
-- memory for that message. If you don't delete a message after it is received,
-- the visibility timeout for the message the next time it is received reverts
-- to the original timeout value, not the value you set with the
-- ChangeMessageVisibility action.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/2012-11-05/APIReference/API_ChangeMessageVisibility.html>
--
data ChangeMessageVisibility = ChangeMessageVisibility
    { cmvReceiptHandle :: !ReceiptHandle
    -- ^ The receipt handle associated with the message whose visibility timeout
    -- should be changed. This parameter is returned by the ReceiveMessage
    -- action.

    , cmvVisibilityTimeout :: !Int
    -- ^ The new value (in seconds - from 0 to 43200 - maximum 12 hours) for the
    -- message's visibility timeout.

    , cmvQueueName :: !QueueName
    -- ^ The URL of the Amazon SQS queue to take action on.
    }
    deriving (Show, Read, Eq, Ord)

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse {}
    deriving (Show, Read, Eq, Ord)

instance ResponseConsumer r ChangeMessageVisibilityResponse where
    type ResponseMetadata ChangeMessageVisibilityResponse = SqsMetadata
    responseConsumer _ _ = sqsXmlResponseConsumer parse
      where
        parse _ = return ChangeMessageVisibilityResponse {}

-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery ChangeMessageVisibility where
    type ServiceConfiguration ChangeMessageVisibility  = SqsConfiguration
    signQuery ChangeMessageVisibility {..} = sqsSignQuery SqsQuery
        { sqsQueueName = Just cmvQueueName
        , sqsQuery =
            [ ("Action", Just "ChangeMessageVisibility")
            , ("ReceiptHandle", Just . TE.encodeUtf8 $ printReceiptHandle cmvReceiptHandle)
            , ("VisibilityTimeout", Just . B.pack $ show cmvVisibilityTimeout)
            ]
        }

instance Transaction ChangeMessageVisibility ChangeMessageVisibilityResponse

instance AsMemoryResponse ChangeMessageVisibilityResponse where
    type MemoryResponse ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse
    loadToMemory = return
